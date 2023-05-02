{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import qualified NegativeRTimed       as OnChain
import           Control.Monad           (mapM, replicateM)
import           Plutus.Model            (Ada (Lovelace), DatumMode (HashDatum),
                                          Run, Tx,
                                          TypedValidator (TypedValidator),
                                          UserSpend, ada, adaValue,
                                          defaultBabbage, initMock, mustFail,
                                          newUser, payToKey, payToScript,
                                          runMock, spend, spendScript, submitTx,
                                          toV2, userSpend, utxoAt, valueAt, waitUntil, currentTimeRad, validateIn)
import           Plutus.V2.Ledger.Api    (PubKeyHash, TxOut (txOutValue),
                                          TxOutRef, Value, POSIXTime (POSIXTime, getPOSIXTime))
import           PlutusTx.Builtins       (Integer, mkI)
import           PlutusTx.Prelude        (Bool (..), Eq ((==)),
                                          return, ($), (&&), (.))
import           Prelude                 (IO, Ord ((<), (>)),
                                          mconcat)
import           Test.QuickCheck         (Property, Testable (property),
                                          collect, (==>), Arbitrary (arbitrary), choose)
import           Test.QuickCheck.Monadic (assert, monadic, run)
import           Test.Tasty              (defaultMain, testGroup)
import           Test.Tasty.QuickCheck   as QC (testProperty)

---------------------------------------------------------------------------------------------------
--------------------------------------- TESTING MAIN ----------------------------------------------

-- | Test the validator script
main :: IO ()
main = defaultMain $ do
    testGroup
      "Testing script properties"
      [ testProperty "Anything before the deadline always fails       " prop_Before_Fails
      , testProperty "Positive redeemer after deadline always fails   " prop_PositiveAfter_Fails
      , testProperty "Negative redeemer after deadline always succeeds" prop_NegativeAfter_Succeeds
      ]

---------------------------------------------------------------------------------------------------
-------------------------------- HELPER FUNCTIONS/INSTANCES ---------------------------------------

-- | Make Run an instance of Testable so we can use it with QuickCheck (we do this because the PlutusSimpleModel library uses (Run a) to run the state monad
      -- THis is basically boilerplate, although we might change the configuration 'defaultBabbage' and the amount of ADA we give to the admin (10_000_000)
instance Testable a => Testable (Run a) where
  property rp = let (a,_) = runMock rp $ initMock defaultBabbage (adaValue 10_000_000) in property a

-- Make POSIXTime an instance of Arbitrary so QuickCheck can generate random values to test
    -- This is also boilerplate but we can also make Arbitrary instances of common types like Int, Integer, ... and choose a different boundary
instance Arbitrary POSIXTime where
  arbitrary = do
    n <- choose (0, 2000) -- choose a random no b/n 0 & 2000
    return (POSIXTime n)  -- return a random POSIX time

-- Time to wait before consumming UTxO from script
waitBeforeConsumingTx :: POSIXTime
waitBeforeConsumingTx = 1000

-- Set many users at once
setupUsers :: Run [PubKeyHash]
setupUsers = replicateM 2 $ newUser $ ada (Lovelace 1000)

-- Validator's script
valScript :: TypedValidator datum redeemer
valScript = TypedValidator $ toV2 OnChain.validator

-- Create transaction that spends "usp" to lock "val" in "giftScript"
lockingTx :: POSIXTime -> UserSpend -> Value -> Tx
lockingTx dl usp val =
  mconcat
    [ userSpend usp
    , payToScript valScript (HashDatum (OnChain.MkCustomDatum dl)) val
    ]

-- Create transaction that spends "giftRef" to unlock "giftVal" from the "valScript" validator
consumingTx :: POSIXTime -> Integer -> PubKeyHash -> TxOutRef -> Value -> Tx
consumingTx dl redeemer usr ref val =
  mconcat
    [ spendScript valScript ref (mkI redeemer) (OnChain.MkCustomDatum dl)
    , payToKey usr val
    ]


---------------------------------------------------------------------------------------------------
------------------------------------- TESTING PROPERTIES ------------------------------------------

-- *** Keep in mind that we always try to consume the UTxO at 1000 POSIX time
-- ** Also, the parameters for the below functions are given randomly by QuickCheck (testProperty)

-- All redeemers fail before deadline 
prop_Before_Fails :: POSIXTime -> Integer -> Property
prop_Before_Fails d r = (d > 1001) ==> runChecks False d r  -- Here, the deadline is above 1000 (it should always fail)

-- Positive redeemer always fail after deadline
prop_PositiveAfter_Fails :: POSIXTime -> Integer -> Property
prop_PositiveAfter_Fails d r = (r > 0 && d < 999) ==> runChecks False d r   -- Here, the redeemer is positive (should always fail)

-- Negative redeemers always succeed after deadline
prop_NegativeAfter_Succeeds :: POSIXTime -> Integer -> Property
prop_NegativeAfter_Succeeds d r = (r < 0 && d < 999) ==> runChecks True d r   -- -ve redeemer and deadline passed (should always succeed)

    -- runChecks :: Bool -> POSIXTime -> Integer -> Property
                  -- Bool => We expect these checks to succeed/fail
                  -- POSIXTime / Integer => provide a random deadline and redeemer
    -- ==> :: Bool -> Property -> Property

---------------------------------------------------------------------------------------------------
------------------------------------- RUNNING THE TESTS -------------------------------------------


-- | Check that the expected and real balances match after using the validator with different redeemers
--     This is mostly boiler plate
runChecks :: Bool -> POSIXTime -> Integer -> Property
runChecks shouldConsume deadline redeemer = 
  collect (redeemer, getPOSIXTime deadline) $ monadic property check
  -- collect :: (Integer, Integer) -> Property -> Property ... collect is used just for seeing which values are generated and tested against our properties
                                                          -- we can run the tests without it if we want shorter logs
    where check = do
            balancesMatch <- run $ testValues shouldConsume deadline redeemer -- get the final balances (in Bool) ... run :: Run Bool -> PropertyM Run Bool
            assert balancesMatch  -- assert :: Bool -> PropertyM Run ()


-- Function to test if both creating an consuming script UTxOs works properly
testValues :: Bool -> POSIXTime -> Integer -> Run Bool
testValues shouldConsume datum redeemer = do
  -- SETUP USERS
  [u1, u2] <- setupUsers
  -- USER 1 LOCKS 100 Lovelaces ("val") IN VALIDATOR
  let val = adaValue 100                    -- Define value to be transfered
  sp <- spend u1 val                        -- Get user's UTXOs that we should spend
  submitTx u1 $ lockingTx datum sp val      -- User 1 submits "lockingTx" transaction
  -- WAIT FOR A BIT
  waitUntil waitBeforeConsumingTx
  -- USER 2 TAKES "val" FROM VALIDATOR
  utxos <- utxoAt valScript                 -- Query blockchain to get all UTxOs at script
  let [(oRef, oOut)] = utxos                -- We know there is only one UTXO (the one we created before)
      tx = consumingTx datum redeemer u2 oRef (txOutValue oOut)            -- Define transaction to be submitted (for consuming the UTxO at the script)
      v2Expected = if shouldConsume then adaValue 1100 else adaValue 1000  -- Define expected balance for user 2 (depending on whether the txn succeeds or not)
  ct  <- currentTimeRad 100                 -- Create time interval with equal radius around current time
  tx' <- validateIn ct tx                   -- Build final consuming Tx
  
  if shouldConsume then submitTx u2 tx' else mustFail . submitTx u2 $ tx'  -- User 2 submits "consumingTx" transaction
    -- The txn should succeed if shouldConsume is true otherwise it should fail (this is tested for a wide range of random values and all these tests should pass
      -- to say that we successfully tested our smart contract)
      
  -- CHECK THAT FINAL BALANCES MATCH EXPECTED BALANCES
  [v1, v2] <- mapM valueAt [u1, u2]               -- Get final balances
  return $ v1 == adaValue 900 && v2 == v2Expected -- Check if final balances match expected balances
      -- The first user should always have 900 because sending to the script is required to consume from it.
      -- The second user should have the expected balance to be sure that the txns have succeeded/failed
            -- These should be the result of the testValues function (True -> all tests passed, False -> all/some tests failed)
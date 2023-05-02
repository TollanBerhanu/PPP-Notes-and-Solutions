{-# LANGUAGE NumericUnderscores #-}

module Main where

import           Prelude
import           Test.Tasty           (defaultMain, testGroup)

import           Control.Monad        (replicateM)
import           Plutus.Model         (Ada (Lovelace), Run, ada, adaValue,
                                       defaultBabbage, mustFail, newUser,
                                       noErrors, sendValue, testNoErrors,
                                       valueAt)
import           Plutus.V1.Ledger.Api (PubKeyHash)

---------------------------------------------------------------------------------------------------
--------------------------------------- TESTING MAIN ----------------------------------------------

main :: IO ()
main = defaultMain $ do
    testGroup   -- allows us to give a name to a list of tests ... testGroup :: TestName -> [TestTree] -> TestTree
      "Test simple user transactions"
      [ good "Simple spend" simpleSpend         -- good "description" actual_test (user_1 sends a txn to user_2 ... txn should succeed)
      , bad  "Not enough funds" notEnoughFunds  -- bad "description" actual_test (user_1 sends an amound exceeding his funds to user_2 ... txn should fail)
      ]
      where
        bad msg = good msg . mustFail   -- Test will pass if 'notEnoughFunds' returns (Run False)
          -- mustFail :: Plutus.Model.Run a -> Plutus.Model.Run ()
        good = testNoErrors (adaValue 10_000_000) defaultBabbage
          -- testNoErrors :: Plutus.V1.Ledger.Value.Value -> MockConfig -> String -> Plutus.Model.Run a -> tasty-1.4.2.3:Test.Tasty.Core.TestTree
              -- adaValue :: Integer -> Plutus.V1.Ledger.Value.Value
              -- defaultBabbage :: MockConfig  ... this is a configuration of our mock blockchain

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------
-- newtype Run a = Run (State Mock a)
--     deriving (Functor, Applicative, Monad, MonadState Mock)

-- Set many users at once (in this case set 3 users each having 1000 lovelace ... 1000 lovaelace is transferred to the users initially)
setupUsers :: Run [PubKeyHash]
setupUsers = replicateM 3 $ newUser $ ada (Lovelace 1000)   -- newUser :: Value -> Plutus.Model.Run Plutus.V1.Ledger.Crypto.PubKeyHash

---------------------------------------------------------------------------------------------------
------------------------------------- TESTING TRANSACTIONS ----------------------------------------

-- Function to test that a simple transaction works
simpleSpend :: Run Bool   -- This is the same as State Mock Bool (Mock is the type of the state and Bool is the return value of each computation)
simpleSpend = do
    users <- setupUsers                -- Create 3 users and assign each 1000 lovelaces
    let [u1, u2, u3] = users           -- Give names to individual users
    sendValue u1 (adaValue 100) u2     -- Send 100 lovelaces from user 1 to user 2
      -- sendValue :: PubKeyHash -> Value -> PubKeyHash -> Run () ... action that sends ada between users and returns unit
    sendValue u2 (adaValue 100) u3     -- Send 100 lovelaces from user 2 to user 3
    isOk <- noErrors                   -- Check that all TXs were accepted without errors 
      -- noErrors :: Plutus.Model.Run Bool
    vals <- mapM valueAt users         -- Read user values, this should return the list of values corresponding to each user
      -- valueAt :: HasAddress user => Run Value
      -- mapM :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b) ... should return a list of 'Value' types
    return $ isOk &&                     -- Check isOk and that all users have correct values
           (vals == fmap adaValue [900, 1000, 1100])

-- Function to test that a transaction fails if there are not enough funds
notEnoughFunds :: Run Bool
notEnoughFunds = do
  users <- setupUsers               -- Create 3 users and assign each 1000 lovelaces
  let [u1, u2, _u3] = users         -- Give names to individual users
  sendValue u1 (adaValue 10000) u2  -- Send 10,000 lovelaces from user 1 to user 2
  noErrors  -- Check that all TXs were accepted without errors (should fail) ... this should return False
    -- noErrors :: Plutus.Model.Run Bool
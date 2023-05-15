{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Staking
    ( stakeValidator
    , saveStakeValidator
    ) where

import           Plutus.V1.Ledger.Value (valueOf)
import           Plutus.V2.Ledger.Api   (Address, BuiltinData,
                                         ScriptContext (scriptContextPurpose, scriptContextTxInfo),
                                         ScriptPurpose (Certifying, Rewarding),
                                         StakeValidator, StakingCredential,
                                         TxInfo (txInfoOutputs, txInfoWdrl),
                                         TxOut (txOutAddress, txOutValue),
                                         adaSymbol, adaToken,
                                         mkStakeValidatorScript)
import qualified PlutusTx
import qualified PlutusTx.AssocMap      as PlutusTx
import           PlutusTx.Prelude       (AdditiveSemigroup ((+)), Bool (..),
                                         Eq ((==)), Integer,
                                         Maybe (Just, Nothing),
                                         MultiplicativeSemigroup ((*)),
                                         Ord ((>=)), Semigroup ((<>)), foldl,
                                         otherwise, traceError, traceIfFalse,
                                         ($), (.))
import           Prelude                (IO, String, ioError)
import           System.IO.Error        (userError)
import           Utilities              (tryReadAddress, wrapStakeValidator,
                                         writeStakeValidatorToFile)

{-# INLINABLE mkStakeValidator #-}
            -- (Parameter..) -> Redemeer -> ScriptContext           ... we don't have a Datum like MintingPolicies
mkStakeValidator :: Address -> () -> ScriptContext -> Bool
mkStakeValidator addr () ctx = case scriptContextPurpose ctx of
    Certifying _   -> True                      -- (Certifying DCert) ... we don't consider the ScriptPurpose that has to do with Certifying
    Rewarding cred -> traceIfFalse "insufficient reward sharing" $ 2 * paidToAddress >= amount cred     -- (Rewarding StakingCredential) ... we check if alteast half
                            -- of the withdrawn rewards is given to the address specified in the parameter (we get the amount withdrawn from the 'txInfoWdrl' field)
    _              -> False                     -- Running this script with any other ScriptPurpose should fail
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    amount :: StakingCredential -> Integer  -- returns the reward amount (lovelace) we are withdrawing of the specific StakingCredential given by this script 
                                            -- (the script we are currently defining)
    amount cred = case PlutusTx.lookup cred $ txInfoWdrl info of    -- txInfoWdrl :: TxInfo -> Map StakingCredential Integer
        Just amt -> amt                                     -- We lookup our StakingCredential given by this script in the ScriptPurpose
        Nothing  -> traceError "withdrawal not found"       -- This can't happen (we are literally defining the script and the script will run when we withdraw rewards)

    paidToAddress :: Integer    
    paidToAddress = foldl f 0 $ txInfoOutputs info  -- txInfoOutputs :: [TxOut] ... list of all outputs in the txn
      where
        f :: Integer -> TxOut -> Integer
        f n o
            | txOutAddress o == addr = n + valueOf (txOutValue o) adaSymbol adaToken  -- Check if the UTxO is getting sent to the specified address and accummulate
            | otherwise              = n                                                                                                -- the ADA value in 'n'


{-# INLINABLE mkWrappedStakeValidator #-}
mkWrappedStakeValidator :: Address -> BuiltinData -> BuiltinData -> ()  -- Convert it to Untyped version of StakeValidator, still to be compiled
mkWrappedStakeValidator = wrapStakeValidator . mkStakeValidator

stakeValidator :: Address -> StakeValidator
stakeValidator addr = mkStakeValidatorScript $    -- Plutus.V2.Ledger.Api (mkStakeValidatorScript) :: CompiledCode (BuiltinData -> BuiltinData -> ()) -> StakeValidator
    $$(PlutusTx.compile [|| mkWrappedStakeValidator ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode addr

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveStakeValidator :: String -> IO ()   -- We give it the address of the person/beneficiary to give >= half the rewards to
saveStakeValidator bech32 = do
    case tryReadAddress bech32 of   -- tryReadAddress :: String -> Maybe Address ... is used to parse a stirng into a plutus Address
        Nothing   -> ioError $ userError $ "Invalid address: " <> bech32    -- Throw an exception
        Just addr -> writeStakeValidatorToFile "./assets/staking.plutus" $ stakeValidator addr

-- After we get the serialized script, we must get the stake address from the script
-- Then we register that stake address and then delegate to a pool ... using scripts/register-and-delegate.sh
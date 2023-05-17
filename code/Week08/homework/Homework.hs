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

module Homework
    ( stakeValidator'
    , saveStakeValidator'
    ) where

import           Plutus.V1.Ledger.Value (valueOf)
import           Plutus.V2.Ledger.Api (Address, BuiltinData, PubKeyHash (PubKeyHash),
                                       ScriptContext (scriptContextTxInfo, scriptContextPurpose), StakeValidator,
                                       mkStakeValidatorScript, StakingCredential, TxOut (txOutValue, txOutAddress),
                                       TxInfo (txInfoOutputs, txInfoWdrl),
                                       adaSymbol, adaToken,
                                    ScriptPurpose (Certifying, Rewarding, Spending), BuiltinByteString)
-- import          Plutus.V1.Ledger.Credential
import qualified PlutusTx
import qualified PlutusTx.AssocMap      as PlutusTx
import           PlutusTx.Prelude     (($), (.), traceIfFalse, otherwise, (&&), Integer, Maybe (Nothing, Just), traceError, foldl, Ord (..),
                                        AdditiveSemigroup ((+)), Bool (..), Eq ((==)), MultiplicativeSemigroup ((*)), Semigroup ((<>)))
import           Prelude              (IO, String, ioError)
import           Utilities            (wrapStakeValidator, writeStakeValidatorToFile, tryReadAddress)
-- import Data.Aeson (Value(Bool))
import           System.IO.Error        (userError)
import Plutus.V2.Ledger.Contexts (txSignedBy)

-- | A staking validator with two parameters, a pubkey hash and an address. The validator
--   should work as follows:
--   1.) The given pubkey hash needs to sign all transactions involving this validator.
--   2.) The given address needs to receive at least half of all withdrawn rewards.
{-# INLINABLE mkStakeValidator' #-}
mkStakeValidator' :: PubKeyHash -> Address -> () -> ScriptContext -> Bool
mkStakeValidator' _pkh _addr () _ctx = traceIfFalse "Only user 1 can delegate or withdraw using this stake validator" True &&
                                       traceIfFalse "You should send atleast half of the rewards to User 2" halfToUser2
    where
        info :: TxInfo
        info = scriptContextTxInfo _ctx

        signedByUser1 :: Bool
        signedByUser1 = txSignedBy (scriptContextTxInfo _ctx) _pkh

        -- getPubKeyCredential :: StakingCredential -> PubKeyHash
        -- getPubKeyCredential StakingCredential _ _ = undefined

        amount :: StakingCredential -> Integer  
        
        amount cred = case PlutusTx.lookup cred $ txInfoWdrl info of    -- txInfoWdrl :: TxInfo -> Map StakingCredential Integer 
            Just amt -> amt
            Nothing  -> traceError "withdrawal not found"       
        
        -- df = StakingCredential _ _ :: StakingCredential

        paidToAddress :: Integer    
        paidToAddress = foldl addAdas 0 $ txInfoOutputs info  -- txInfoOutputs :: [TxOut] ... list of all outputs in the txn
            where
                addAdas :: Integer -> TxOut -> Integer
                addAdas adas' txOut'
                    | txOutAddress txOut' == _addr = adas' + valueOf (txOutValue txOut') adaSymbol adaToken  -- add all Ada values getting sent to User 2
                    | otherwise              = adas'                                                                                            

        halfToUser2 :: Bool
        halfToUser2 = case scriptContextPurpose _ctx of
                        Certifying _   -> True                      -- (Certifying DCert) ... we don't consider the ScriptPurpose that has to do with Certifying
                        Rewarding cred -> 2 * paidToAddress >= amount cred     -- (Rewarding StakingCredential) ...  alteast half of the rewards is given to User 2
                        Spending _     -> signedByUser1    
                        _              -> False    

{-# INLINABLE mkWrappedStakeValidator' #-}
mkWrappedStakeValidator' :: PubKeyHash -> Address -> BuiltinData -> BuiltinData -> ()
mkWrappedStakeValidator' pkh = wrapStakeValidator . mkStakeValidator' pkh

stakeValidator' :: PubKeyHash -> Address -> StakeValidator
stakeValidator' pkh addr = mkStakeValidatorScript $
    $$(PlutusTx.compile [|| mkWrappedStakeValidator' ||])
        `PlutusTx.applyCode` PlutusTx.liftCode pkh
        `PlutusTx.applyCode` PlutusTx.liftCode addr

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveStakeValidator' :: BuiltinByteString -> String -> IO ()
saveStakeValidator' _pkh _bech32 = case tryReadAddress _bech32 of   
        Nothing   -> ioError $ userError $ "Invalid address: " <> _bech32 
        Just addr -> writeStakeValidatorToFile "./assets/staking-homework.plutus" $ stakeValidator' (PubKeyHash _pkh) addr

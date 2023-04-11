{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Homework2 where

import           Plutus.V2.Ledger.Api (BuiltinData, POSIXTime, PubKeyHash,
                                       ScriptContext (scriptContextTxInfo), Validator,
                                       mkValidatorScript, TxInfo (txInfoValidRange))
import           Plutus.V2.Ledger.Contexts      (txSignedBy)
import           PlutusTx             (applyCode, compile, liftCode)
import           PlutusTx.Prelude     ((.), traceIfFalse, (&&), Bool(..))
import           Utilities            (wrapValidator)
import Plutus.V1.Ledger.Interval

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

{-# INLINABLE mkParameterizedVestingValidator #-}
-- This should validate if the transaction has a signature from the parameterized beneficiary and the deadline has passed.
mkParameterizedVestingValidator :: PubKeyHash -> POSIXTime -> () -> ScriptContext -> Bool
mkParameterizedVestingValidator _beneficiary _deadline () _ctx = traceIfFalse "Beneficiary hasn't signed" beneficiarySigned &&
                                                                 traceIfFalse "Deadline hasn't passed" deadline_passed
    where info :: TxInfo
          info = scriptContextTxInfo _ctx

          beneficiarySigned = txSignedBy info _beneficiary
          deadline_passed = from _deadline `contains` txInfoValidRange info

{-# INLINABLE  mkWrappedParameterizedVestingValidator #-}
mkWrappedParameterizedVestingValidator :: PubKeyHash -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedParameterizedVestingValidator = wrapValidator . mkParameterizedVestingValidator

validator :: PubKeyHash -> Validator
validator beneficiary = mkValidatorScript ($$(compile [|| mkWrappedParameterizedVestingValidator ||]) `applyCode` liftCode beneficiary)

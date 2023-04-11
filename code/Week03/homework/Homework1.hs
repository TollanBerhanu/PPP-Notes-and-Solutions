{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module Homework1 where

import           Plutus.V2.Ledger.Api      (BuiltinData, POSIXTime, PubKeyHash,
                                            ScriptContext (scriptContextTxInfo),
                                            TxInfo (txInfoValidRange),
                                            Validator, mkValidatorScript)
import           Plutus.V2.Ledger.Contexts      (txSignedBy)
import           PlutusTx             (compile, unstableMakeIsData)
import           PlutusTx.Prelude     (Bool (..), traceIfFalse, ($), (&&), (||))
import           Utilities            (wrapValidator)
import Plutus.V1.Ledger.Interval (contains, to, before)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

data VestingDatum = VestingDatum
    { beneficiary1 :: PubKeyHash
    , beneficiary2 :: PubKeyHash
    , deadline     :: POSIXTime
    }

unstableMakeIsData ''VestingDatum

{-# INLINABLE mkVestingValidator #-}
-- This should validate if either beneficiary1 has signed the transaction and the current slot is before or at the deadline
-- or if beneficiary2 has signed the transaction and the deadline has passed.
mkVestingValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkVestingValidator _dat () _ctx = (traceIfFalse "Beneficiary 1 hasn't signed " beneficiary1_Signed &&
                                  traceIfFalse "Deadline has passed " deadline_not_passed) ||
                                  (traceIfFalse "Beneficiary 2 hasn't signed " beneficiary2_Signed &&
                                  traceIfFalse "Deadline hasn't passed" deadline_passed)

    where info :: TxInfo
          info = scriptContextTxInfo _ctx

          beneficiary1_Signed = txSignedBy info $ beneficiary1 _dat
          beneficiary2_Signed = txSignedBy info $ beneficiary2 _dat
          deadline_passed = deadline _dat `before` txInfoValidRange info
          deadline_not_passed = to (deadline _dat) `contains` txInfoValidRange info

{-# INLINABLE  mkWrappedVestingValidator #-}
mkWrappedVestingValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedVestingValidator = wrapValidator mkVestingValidator

validator :: Validator
validator = mkValidatorScript $$(compile [|| mkWrappedVestingValidator ||])

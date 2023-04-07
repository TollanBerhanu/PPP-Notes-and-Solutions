{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}

module CustomTypes where

import qualified Plutus.V2.Ledger.Api as PlutusV2
import           PlutusTx             (BuiltinData, compile, unstableMakeIsData)
import           PlutusTx.Prelude     (Bool, Eq ((==)), Integer, traceIfFalse,
                                       ($))
import           Prelude              (IO)
import           Utilities            (wrapValidator, writeValidatorToFile)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

-- We can create custom data types for our datum and redeemer like this:
newtype MySillyRedeemer = MkMySillyRedeemer Integer
PlutusTx.unstableMakeIsData ''MySillyRedeemer -- Use TH to create an instance for IsData.
-- You have more control over how exactly values are deserialiezed in the stable (but you should provide more detail)
-- this will automatically create instances for UnsafeFromData, FromData and ToData (check by :i MySillyRedeemer)
--                                   unsafeFromBuiltinData   fromBuiltinData  toBuiltinData
-- This validator succeeds only if the redeemer is `MkMySillyRedeemer 42`
--              Datum     Redeemer            ScriptContext
mkCTValidator :: () -> MySillyRedeemer -> PlutusV2.ScriptContext -> Bool
mkCTValidator _ (MkMySillyRedeemer r) _ = traceIfFalse "expected 42" $ r == 42
{-# INLINABLE mkCTValidator #-}

wrappedMkVal :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedMkVal = wrapValidator mkCTValidator
{-# INLINABLE wrappedMkVal #-}
-- wrapValidator works automatically because MySillyRedeemer is made an instance of UnsafeFromData

{-  Prelude Utilities CustomTypes> writeDataToFile "silly.json" $ MkMySillyRedeemer 42
    Wrote data to: silly.json
    {
        "constructor": 0,
        "fields": [
            {
                "int": 42
            }
        ]
    } -}
{-  Prelude Utilities CustomTypes> writeDataToFile "pair.json" $ ([12 :: Integer, 34 :: Integer], True)
    Wrote data to: pair.json

    {   "constructor": 0,
        "fields": [
            {
                "list": [
                    {
                        "int": 12
                    },
                    {
                        "int": 34
                    }
                ]
            },
            {
                "constructor": 1,
                "fields": []
            }
        ]
    } 
-}

validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| wrappedMkVal ||])

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveVal :: IO ()
saveVal = writeValidatorToFile "./assets/customtypes.plutus" validator

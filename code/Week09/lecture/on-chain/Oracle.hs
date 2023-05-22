{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings  #-}

module Oracle where

import Plutus.V2.Ledger.Api
    ( BuiltinData,
      ScriptContext(scriptContextTxInfo),
      mkValidatorScript,
      PubKeyHash,
      Datum(Datum),
      Validator,
      TxInInfo(txInInfoResolved),
      TxInfo,
      OutputDatum(OutputDatumHash, NoOutputDatum, OutputDatum),
      TxOut(txOutDatum, txOutValue), UnsafeFromData (unsafeFromBuiltinData) )
import Plutus.V2.Ledger.Contexts
    ( findDatum,
      getContinuingOutputs,
      txSignedBy,
      findOwnInput )    
import PlutusTx
    ( compile,
      unstableMakeIsData,
      FromData(fromBuiltinData),
      liftCode,
      applyCode,
      makeLift, CompiledCode )
import PlutusTx.Prelude
    ( Bool,
      Integer,
      Maybe(..),
      ($),
      (.),
      (&&),
      tail,
      isJust,
      traceError,
      traceIfFalse,
      Eq(..), 
      take 
      )
import           Prelude                    (Show (show), span, IO)
import qualified  Prelude               ((/=) )
import Data.String ( IsString(fromString), String )
import Plutus.V1.Ledger.Value
    ( assetClassValueOf, AssetClass(AssetClass) )
import           Utilities            (wrapValidator, writeValidatorToFile, writeCodeToFile)
import Text.Printf (printf)

---------------------------------------------------------------------------------------------------
----------------------------- ON-CHAIN: HELPER FUNCTIONS/TYPES ------------------------------------

{-# INLINABLE parseOracleDatum #-}
parseOracleDatum :: TxOut -> TxInfo -> Maybe Integer    --  We check and parse the Datum of the output UTxO
parseOracleDatum o info = case txOutDatum o of
    NoOutputDatum -> Nothing
    OutputDatum (Datum d) -> PlutusTx.fromBuiltinData d     -- Inline datum: just parse the datum (check if it is an Integer)
    OutputDatumHash dh -> do
                        Datum d <- findDatum dh info        -- Datum's hash: find the actual datum from the ScriptContext
                        PlutusTx.fromBuiltinData d          --               parse the datum

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

data OracleParams = OracleParams    -- These parameters never change for the Oracle
    { oNFT        :: AssetClass             -- The oracle can only use one NFT
    , oOperator   :: PubKeyHash             -- The oracle can be operated by only one person
    } 
PlutusTx.makeLift ''OracleParams

data OracleRedeemer = Update | Delete     -- Once the OracleValidator is minted, it can only be updated/deleted
    deriving Prelude.Show
PlutusTx.unstableMakeIsData ''OracleRedeemer

-- Oracle Datum (price of ADA in USD ... in cents)
type Rate = Integer

{-# INLINABLE mkValidator #-}
mkValidator :: OracleParams -> Rate -> OracleRedeemer -> ScriptContext -> Bool
mkValidator oracle _ r ctx =    -- We don't use the Rate (Datum) in this validator ... we read the rate as a reference input in other validators
    case r of
        Update -> traceIfFalse "token missing from input"   inputHasToken  &&           -- We must consume the prev Oracle with the NFT
                  traceIfFalse "token missing from output"  outputHasToken &&            
                  traceIfFalse "operator signature missing" checkOperatorSignature &&
                  traceIfFalse "invalid output datum"       validOutputDatum            -- The datum of the o/p UTxO must be an Integer
        Delete -> traceIfFalse "operator signature missing" checkOperatorSignature

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- | Check that the 'oracle' is signed by the 'oOperator'.
    checkOperatorSignature :: Bool
    checkOperatorSignature = txSignedBy info $ oOperator oracle

    -- | Find the oracle input.
    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
        Nothing -> traceError "oracle input missing"
        Just i  -> txInInfoResolved i

    -- Check that the oracle input contains the NFT.
    inputHasToken :: Bool
    inputHasToken = assetClassValueOf (txOutValue ownInput) (oNFT oracle) == 1  -- Check that the AssetClass of the NFT has amount == 1

    -- | Find the oracle output.
    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
        [o] -> o
        _   -> traceError "expected exactly one oracle output"

    -- Check that the oracle output contains the NFT.
    outputHasToken :: Bool
    outputHasToken = assetClassValueOf (txOutValue ownOutput) (oNFT oracle) == 1

    -- Check that the oracle output contains a valid datum.
    validOutputDatum :: Bool
    validOutputDatum = isJust $ parseOracleDatum ownOutput info


---------------------------------------------------------------------------------------------------
------------------------------------ COMPILE VALIDATOR --------------------------------------------


{-# INLINABLE  mkWrappedValidator #-}
mkWrappedValidator :: OracleParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator = wrapValidator . mkValidator


validator :: OracleParams -> Validator
validator oracle = mkValidatorScript $
    $$(PlutusTx.compile [|| mkWrappedValidator ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode oracle


{-# INLINABLE  mkWrappedValidatorLucid #-}
--                            CS              TN           operator        rate          redeemer       context
mkWrappedValidatorLucid :: BuiltinData ->  BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidatorLucid cs tn pkh = wrapValidator $ mkValidator op
    where
        op = OracleParams   -- Parse the parameters to apply them to the mkValidator function
            { oNFT = AssetClass (unsafeFromBuiltinData cs, unsafeFromBuiltinData tn)    -- Build the AssetClass from the 'CurrencySymbol' and the 'TokenName'
            , oOperator   = unsafeFromBuiltinData pkh       -- Convert pkh from BuiltinData to PubKeyHash
            }

validatorCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
validatorCode = $$( compile [|| mkWrappedValidatorLucid ||])

---------------------------------------------------------------------------------------------------
------------------------------------- SAVE VALIDATOR -------------------------------------------

saveOracleCode :: IO ()
saveOracleCode = writeCodeToFile "assets/oracle.plutus" validatorCode

saveOracleScript :: String -> PubKeyHash -> IO ()
saveOracleScript symbol pkh = do
    let
    writeValidatorToFile fp $ validator op
    where
        op = OracleParams
            { oNFT= parseToken symbol
            , oOperator   = pkh
            }
        fp = printf "assets/oracle-%s-%s.plutus" (take 3 (show pkh)) $ take 3 (show pkh)

parseToken :: String -> AssetClass
parseToken s =
  let
    (x, y) = span (Prelude./= '.') s
  in
    AssetClass (fromString x, fromString $ tail y)
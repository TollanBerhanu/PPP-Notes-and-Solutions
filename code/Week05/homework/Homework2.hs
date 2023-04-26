{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Homework2 where

import           Plutus.V1.Ledger.Value     (flattenValue)
import           Plutus.V2.Ledger.Api       (BuiltinData, MintingPolicy,
                                            ScriptContext(scriptContextTxInfo), TokenName(TokenName), unTokenName, TxOutRef,
                                            mkMintingPolicyScript, TxInfo(txInfoInputs, txInfoMint),
                                            TxInInfo (txInInfoOutRef), TxOutRef (TxOutRef), TxId(TxId))
import           Prelude                    (IO)                                             
import qualified PlutusTx
import           PlutusTx.Prelude           (Bool (False), traceIfFalse, ($), (.), (&&), Eq ((==)), any)
import           Utilities                  (wrapPolicy, writeCodeToFile)

{-# INLINABLE mkEmptyNFTPolicy #-}
-- Minting policy for an NFT, where the minting transaction must consume the given UTxO as input
-- and where the TokenName will be the empty ByteString.
mkEmptyNFTPolicy :: TxOutRef -> () -> ScriptContext -> Bool
mkEmptyNFTPolicy _oref () _ctx = traceIfFalse "Missing UTxO" hasUTxO && -- FIX ME!
                                 traceIfFalse "Wrong amount and/or TokenName" checkMintedAmount
    where
        info :: TxInfo
        info = scriptContextTxInfo _ctx

        hasUTxO :: Bool
        hasUTxO = any (\i -> txInInfoOutRef i == _oref) $ txInfoInputs info 

        checkMintedAmount :: Bool 
        checkMintedAmount = case flattenValue (txInfoMint info) of   -- flattenValue :: Value -> [(CurrencySymbol, TokenName, Integer)]
            [(_, tn, amt)] -> unTokenName tn == "" && amt == 1  -- check if the TokenName is the same and if the amount is 1
            _              -> False 

{-# INLINABLE mkWrappedEmptyNFTPolicy #-}
                         -- TxId -> Idx -> BuiltinData -> BuiltinData -> ()
mkWrappedEmptyNFTPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedEmptyNFTPolicy tid ix = wrapPolicy $ mkEmptyNFTPolicy _oref 
  where
    _oref :: TxOutRef
    _oref = TxOutRef -- compute the TxOutRef from TxId and Idx ... TxOutRef :: TxId -> Integer -> TxOutRef
        (TxId $ PlutusTx.unsafeFromBuiltinData tid) -- convert the Builtindata back to BuiltinByteString ... TxId :: BuiltinByteString -> TxId
        (PlutusTx.unsafeFromBuiltinData ix)         -- convert the Builtindata back to Integer

-- This yields the parameterized script (script that expects parameters) ... still not a complete minting policy
emptyNftCode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
emptyNftCode = $$(PlutusTx.compile [|| mkWrappedEmptyNFTPolicy ||])

saveEmptyNFTCode :: IO ()
saveEmptyNFTCode = writeCodeToFile "assets/empty_nft.plutus" emptyNftCode

{-
{-# INLINABLE mkWrappedEmptyNFTPolicy #-}
mkWrappedEmptyNFTPolicy :: TxOutRef -> BuiltinData -> BuiltinData -> ()
mkWrappedEmptyNFTPolicy = wrapPolicy . mkEmptyNFTPolicy

nftPolicy :: TxOutRef -> TokenName -> MintingPolicy
nftPolicy oref tn = mkMintingPolicyScript $ $$(PlutusTx.compile [|| mkWrappedEmptyNFTPolicy ||]) `PlutusTx.applyCode` PlutusTx.liftCode oref
-}
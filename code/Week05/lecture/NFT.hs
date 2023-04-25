{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module NFT where

import qualified Data.ByteString.Char8      as BS8
import           Plutus.V1.Ledger.Value     (flattenValue)
import           Plutus.V2.Ledger.Api       (BuiltinData, CurrencySymbol,
                                             MintingPolicy,
                                             ScriptContext (scriptContextTxInfo),
                                             TokenName (unTokenName),
                                             TxId (TxId, getTxId),
                                             TxInInfo (txInInfoOutRef),
                                             TxInfo (txInfoInputs, txInfoMint),
                                             TxOutRef (TxOutRef, txOutRefId, txOutRefIdx),
                                             mkMintingPolicyScript)
import qualified PlutusTx
import           PlutusTx.Builtins.Internal (BuiltinByteString (BuiltinByteString))
import           PlutusTx.Prelude           (Bool (False), Eq ((==)), any,
                                             traceIfFalse, ($), (&&))
import           Prelude                    (IO, Show (show), String)
import           Text.Printf                (printf)
import           Utilities                  (bytesToHex, currencySymbol,
                                             wrapPolicy, writeCodeToFile,
                                             writePolicyToFile)

{-# INLINABLE mkNFTPolicy #-}
                -- Param1 -> Param2 -> Redeemer -> Context
mkNFTPolicy :: TxOutRef -> TokenName -> () -> ScriptContext -> Bool  -- our minting policy takes 2 params: UTxO and TokenName
mkNFTPolicy oref tn () ctx = traceIfFalse "UTxO not consumed"   hasUTxO           &&
                             traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool -- check if the specified UTxO is consumed by the txn 
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info -- get all input UTxOs and check whether any of them is our UTxO
                                                    -- txInfoInputs :: TxInfo -> [TxInInfo]
                                                    -- txInInfoOutRef :: TxInInfo -> TxOutRef

    checkMintedAmount :: Bool -- check if the amount of tokens (NFTs) minted is only ONE.
    checkMintedAmount = case flattenValue (txInfoMint info) of   -- flattenValue :: Value -> [(CurrencySymbol, TokenName, Integer)]
        [(_, tn'', amt)] -> tn'' == tn && amt == 1  -- check if the TokenName is the same and if the amount is 1
        _                -> False 

{-# INLINABLE mkWrappedNFTPolicy #-}
-- We make the params' type BuiltinData because thats what is required in lucid. We also used two params instead of TxOutRef so that 
-- we can pass the TxHash and the Index as different parameters (also required by lucid because we can't directly convert Builtindata to
-- TxOutRef)
                         -- TxId -> Idx -> TokenName -> BuiltinData -> BuiltinData -> ()
mkWrappedNFTPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedNFTPolicy tid ix tn' = wrapPolicy $ mkNFTPolicy oref tn
  where
    oref :: TxOutRef
    oref = TxOutRef -- compute the TxOutRef from TxId and Idx ... TxOutRef :: TxId -> Integer -> TxOutRef
        (TxId $ PlutusTx.unsafeFromBuiltinData tid) -- convert the Builtindata back to BuiltinByteString ... TxId :: BuiltinByteString -> TxId
        (PlutusTx.unsafeFromBuiltinData ix)         -- convert the Builtindata back to Integer

    tn :: TokenName
    tn = PlutusTx.unsafeFromBuiltinData tn'   -- convert the Builtindata back to TokenName ... TokenName :: BuiltinByteString -> TokenName
-- Both TxId and TokenName are newType wrappers around BuiltinBytestring but they are deserialized differently, for some reason.

-- This yields the parameterized script (script that expects parameters) ... still not a complete minting policy
nftCode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
nftCode = $$(PlutusTx.compile [|| mkWrappedNFTPolicy ||])

-- This yields the actual minting policy by applying the parameters
nftPolicy :: TxOutRef -> TokenName -> MintingPolicy
nftPolicy oref tn = mkMintingPolicyScript $
    nftCode
        `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData $ getTxId $ txOutRefId oref)
        `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData $ txOutRefIdx oref)
        `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData tn)

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------
-- Save the parameterized script to file
saveNFTCode :: IO ()
saveNFTCode = writeCodeToFile "assets/nft.plutus" nftCode

-- save the minting policy to file after accepting the TxOutRef and TokenName on the on-chain side
saveNFTPolicy :: TxOutRef -> TokenName -> IO ()
saveNFTPolicy oref tn = writePolicyToFile
    (printf "assets/nft-%s#%d-%s.plutus"
        (show $ txOutRefId oref)
        (txOutRefIdx oref) $
        tn') $
    nftPolicy oref tn
  where
    tn' :: String
    tn' = case unTokenName tn of
        (BuiltinByteString bs) -> BS8.unpack $ bytesToHex bs

-- Show the CurrencySymbol after accepting parameters
nftCurrencySymbol :: TxOutRef -> TokenName -> CurrencySymbol
nftCurrencySymbol oref tn = currencySymbol $ nftPolicy oref tn

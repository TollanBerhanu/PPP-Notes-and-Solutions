{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Signed where

import           Plutus.V2.Ledger.Api      (BuiltinData, CurrencySymbol,
                                            MintingPolicy, PubKeyHash,
                                            ScriptContext (scriptContextTxInfo),
                                            mkMintingPolicyScript)
import           Plutus.V2.Ledger.Contexts (txSignedBy)
import qualified PlutusTx
import           PlutusTx.Prelude          (Bool, traceIfFalse, ($), (.))
import           Prelude                   (IO, Show (show))
import           Text.Printf               (printf)
import           Utilities                 (currencySymbol, wrapPolicy,
                                            writeCodeToFile, writePolicyToFile)

{-# INLINABLE mkSignedPolicy #-}
mkSignedPolicy :: PubKeyHash -> () -> ScriptContext -> Bool
mkSignedPolicy pkh () ctx = traceIfFalse "missing signature" $ txSignedBy (scriptContextTxInfo ctx) pkh

{-# INLINABLE mkWrappedSignedPolicy #-}
mkWrappedSignedPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> () -- we used BuiltinData as the first param instead of PubKeyHash
    -- because we can only use parameters of type BuiltinData in lucid
mkWrappedSignedPolicy pkh = wrapPolicy (mkSignedPolicy $ PlutusTx.unsafeFromBuiltinData pkh)
                                    -- we used unsafeFromBuiltinData to convert from BuiltinData to PubKeyHash to feed it to mkSignedPolicy

-- Here we did the compilation in two steps: compilation and application of the parameters to get two versions of the serialized script
signedCode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
signedCode = $$(PlutusTx.compile [|| mkWrappedSignedPolicy ||]) -- mkWrappedSignedPolicy expects an additional parameter, we compile this
    -- function without the parameter so that we can apply the param on the off-chain side (using lucid)

signedPolicy :: PubKeyHash -> MintingPolicy -- add a parameter (pubKeyHash) to the MintingPolicy when running this function on-chain
signedPolicy pkh = mkMintingPolicyScript $ signedCode `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData pkh)
                                            -- we convert PubKeyHash to BuiltinData when we use it as a parameter on the on-chain code
---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

-- This writes the serialized script where the parameter hasn't been applied yet, so we can use lucid (off-chain code) to apply the 
    -- parameter. Keep in mind that it's not a complete minting policy until we apply the parameter
saveSignedCode :: IO ()
saveSignedCode = writeCodeToFile "assets/signed.plutus" signedCode

-- This takes a PubKeyHash and serialized the minting policy based on that
saveSignedPolicy :: PubKeyHash -> IO ()
saveSignedPolicy pkh = writePolicyToFile (printf "assets/signed-%s.plutus" $ show pkh) $ signedPolicy pkh

-- Hash the minting policy to get the CurrencySymbol
signedCurrencySymbol :: PubKeyHash -> CurrencySymbol
signedCurrencySymbol = currencySymbol . signedPolicy
-- The currency symbols will be different for each wallet
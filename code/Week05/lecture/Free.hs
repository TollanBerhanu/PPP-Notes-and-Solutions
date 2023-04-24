{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Free where

import           Plutus.V2.Ledger.Api (BuiltinData, CurrencySymbol,
                                       MintingPolicy, ScriptContext,
                                       mkMintingPolicyScript)
import qualified PlutusTx
import           PlutusTx.Prelude     (Bool (True))
import           Prelude              (IO)
import           Utilities            (currencySymbol, wrapPolicy,
                                       writePolicyToFile)

{-# INLINABLE mkFreePolicy #-}
            -- Redeemer -> Context ... No datum in minting policies, but we can add parameters (Params -> Redeemer -> Context -> Bool)
mkFreePolicy :: () -> ScriptContext -> Bool     -- Typed version, we don't need a toData instance for the redeemer because it's just ()
mkFreePolicy () _ = True -- Allow any minting/burning for the CurrencySymbol given by this policy (hash of this minting script)

{-# INLINABLE mkWrappedFreePolicy #-} -- For template haskell, to include all the implementation in the oxford brackets (when we compile it to plutus script)
mkWrappedFreePolicy :: BuiltinData -> BuiltinData -> ()   -- Helper function to convert the typed policy to Untyped using wrapPolicy
mkWrappedFreePolicy = wrapPolicy mkFreePolicy

freePolicy :: MintingPolicy
-- freePolicy = mkMintingPolicyScript $$(PlutusTx.compile [|| wrapPolicy mkFreePolicy ||]) -- without using the above function
freePolicy = mkMintingPolicyScript $$(PlutusTx.compile [|| mkWrappedFreePolicy ||]) -- Compile it to plutus core (serialized version)
---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveFreePolicy :: IO ()
saveFreePolicy = writePolicyToFile "assets/free.plutus" freePolicy -- write the serialized minting policy to file

freeCurrencySymbol :: CurrencySymbol
freeCurrencySymbol = currencySymbol freePolicy -- get the CurrencySymbol (hash of the serialized minting policy )

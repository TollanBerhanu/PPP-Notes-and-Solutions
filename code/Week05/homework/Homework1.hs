{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

module Homework1 where

import           Plutus.V1.Ledger.Interval (after)
import           Plutus.V2.Ledger.Api (BuiltinData, MintingPolicy, POSIXTime, TxInfo (txInfoValidRange),
                                       PubKeyHash, ScriptContext(scriptContextTxInfo),
                                       mkMintingPolicyScript)
import           Plutus.V2.Ledger.Contexts (txSignedBy)                                       
import qualified PlutusTx
import           PlutusTx.Prelude     (Bool (False), ($), (&&), traceIfFalse)
import           Prelude              (IO, Show (show))
import           Text.Printf          (printf)
import           Utilities            (wrapPolicy, writeCodeToFile)

{-# INLINABLE mkDeadlinePolicy #-}
-- This policy should only allow minting (or burning) of tokens if the owner of the specified PubKeyHash
-- has signed the transaction and if the specified deadline has not passed.
mkDeadlinePolicy :: PubKeyHash -> POSIXTime -> () -> ScriptContext -> Bool
mkDeadlinePolicy _pkh _deadline () _ctx = traceIfFalse "Owner hasn't signed" ownerSigned && -- FIXED!
                                          traceIfFalse "Deadline has passed" deadlineNotReached
    where
        info :: TxInfo
        info = scriptContextTxInfo _ctx
        
        ownerSigned :: Bool
        ownerSigned = txSignedBy info _pkh

        deadlineNotReached :: Bool
        deadlineNotReached = _deadline `after` (txInfoValidRange info) 

-- ====== This is the way to apply the arguments off-chain

{-# INLINABLE mkWrappedDeadlinePolicy #-}
mkWrappedDeadlinePolicy :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedDeadlinePolicy pkh deadline = wrapPolicy $ mkDeadlinePolicy (PlutusTx.unsafeFromBuiltinData pkh) (PlutusTx.unsafeFromBuiltinData deadline)

deadlineCode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
deadlineCode = $$(PlutusTx.compile [|| mkWrappedDeadlinePolicy ||])

saveDeadlineCode :: IO ()
saveDeadlineCode = writeCodeToFile "assets/deadline.plutus" deadlineCode

{- 
-- ====== This is the way to apply the arguments on-chain

    {-# INLINABLE mkWrappedDeadlinePolicy #-}
    mkWrappedDeadlinePolicy :: PubKeyHash -> POSIXTime -> BuiltinData -> BuiltinData -> ()
    mkWrappedDeadlinePolicy pkh deadline = wrapPolicy $ mkDeadlinePolicy pkh deadline

    deadlinePolicy :: PubKeyHash -> POSIXTime -> MintingPolicy
    deadlinePolicy pkh deadline = mkMintingPolicyScript $
        $$(PlutusTx.compile [|| mkWrappedDeadlinePolicy ||])
            `PlutusTx.applyCode` PlutusTx.liftCode pkh
            `PlutusTx.applyCode` PlutusTx.liftCode deadline

    saveDeadlinePolicy :: PubKeyHash -> POSIXTime -> IO ()
    saveDeadlinePolicy pkh deadline = writePolicyToFile (printf "assets/deadline-%s.plutus" $ show pkh) $ deadlinePolicy pkh deadline
-}
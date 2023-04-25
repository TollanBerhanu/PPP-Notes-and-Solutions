{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Week05.NFT where

import           Control.Monad          hiding (fmap)
import           Data.Aeson             (FromJSON, ToJSON)
import qualified Data.Map               as Map
import           Data.Text              (Text)
import           Data.Void              (Void)
import           GHC.Generics           (Generic)
import           Plutus.Contract        as Contract
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Prelude                (IO, Semigroup (..), Show (..), String)
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet

-- ============================== On-Chain code ======================================

{-# INLINABLE mkPolicy #-}
mkPolicy :: TxOutRef -> TokenName -> () -> ScriptContext -> Bool
mkPolicy oref tn () ctx = traceIfFalse "UTxO not consumed"   hasUTxO           &&
                          traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
        [(_, tn', amt)] -> tn' == tn && amt == 1
        _               -> False

-- create the minting policy with the available parameters
policy :: TxOutRef -> TokenName -> Scripts.MintingPolicy
policy oref tn = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \oref' tn' -> Scripts.wrapMintingPolicy $ mkPolicy oref' tn' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref
    `PlutusTx.applyCode`
    PlutusTx.liftCode tn

curSymbol :: TxOutRef -> TokenName -> CurrencySymbol
curSymbol oref tn = scriptCurrencySymbol $ policy oref tn

-- ============================== Off-Chain code ======================================
data NFTParams = NFTParams  -- define the type of the parameters
    { npToken   :: !TokenName   -- the token name
    , npAddress :: !Address     -- our public address
    } deriving (Generic, FromJSON, ToJSON, Show)

type NFTSchema = Endpoint "mint" NFTParams

mint :: NFTParams -> Contract w NFTSchema Text () -- define our Contract Monad
mint np = do
    utxos <- utxosAt $ npAddress np -- get all UTxOs at our address ... utxosAt :: addr -> Map TxOutRef sth
    case Map.keys utxos of -- we map the keys of utxos (TxOutRefs)
        []       -> Contract.logError @String "no utxo found"
        oref : _ -> do -- take the first TxOutRef from the list and execute the do block
            let tn      = npToken np -- extract the token name from the parameter
            let val     = Value.singleton (curSymbol oref tn) tn 1 -- Value.singleton :: CurrencySymbol -> TokenName -> Integer -> Value
                                        -- CurrencySymbol is computed using curSymbol defined above (on the on-chain code)
                lookups = Constraints.mintingPolicy (policy oref tn) <> Constraints.unspentOutputs utxos
                    -- we specify the minting policy and provide the UTxOs as lookups
                tx      = Constraints.mustMintValue val <> Constraints.mustSpendPubKeyOutput oref
                    -- we can specify the Value and the specific UTxO as constraints
            ledgerTx <- submitTxConstraintsWith @Void lookups tx -- submit the Txn
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx   -- wait for confirmation
            Contract.logInfo @String $ printf "forged %s" (show val) -- log the Value

-- Specify the endpoints
endpoints :: Contract () NFTSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = awaitPromise $ endpoint @"mint" mint

-- ===================== To test the code using the Emulator Monad

test :: IO ()
test = runEmulatorTraceIO $ do
    let tn = "ABC"
        w1 = knownWallet 1
        w2 = knownWallet 2
    h1 <- activateContractWallet w1 endpoints
    h2 <- activateContractWallet w2 endpoints
    callEndpoint @"mint" h1 $ NFTParams -- call "mint" on wallet 1
        { npToken   = tn
        , npAddress = mockWalletAddress w1 -- check if the wallet is a mock wallet (knownWallet 1 - 10) and return the address, 
        }                                       -- else, return an error if it's a real wallet
    callEndpoint @"mint" h2 $ NFTParams
        { npToken   = tn
        , npAddress = mockWalletAddress w2 -- call "mint" on wallet 2
        }
    void $ Emulator.waitNSlots 1
    -- The CurrencySymbol will be different for both wallets (two different NFTs for each wallet)

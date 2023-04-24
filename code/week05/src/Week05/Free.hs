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

module Week05.Free where

import           Control.Monad          hiding (fmap)
import           Data.Aeson             (ToJSON, FromJSON)
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
import           Playground.Contract    (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types       (KnownCurrency (..))
import           Prelude                (IO, Show (..), String)
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet

-- ========================= ON-CHAIN CODE =================================
{-# INLINABLE mkPolicy #-}
mkPolicy :: () -> ScriptContext -> Bool
mkPolicy () _ = True

policy :: Scripts.MintingPolicy
policy = mkMintingPolicyScript $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy mkPolicy ||])

curSymbol :: CurrencySymbol
curSymbol = scriptCurrencySymbol policy

-- ========================= OFF-CHAIN CODE =================================
data MintParams = MintParams
    { mpTokenName :: !TokenName -- "MyTokenName"
    , mpAmount    :: !Integer   -- Amount to mint/burn
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type FreeSchema = Endpoint "mint" MintParams

-- We only have one endpoint (mint), by which we specify the TokenName and the amount we want to forge (+ve amount) or burn (-ve amount)
-- We only specify the TokenName because the CurrencySymbol is given by this script
mint :: MintParams -> Contract w FreeSchema Text ()
mint mp = do
    let val     = Value.singleton curSymbol (mpTokenName mp) (mpAmount mp) -- Value/Native_Token we want to mint/burn 
                        -- singleton :: CurrencySymbol -> TokenName -> Integer -> Value
        lookups = Constraints.mintingPolicy policy -- specify the serialized minting policy script  
        tx      = Constraints.mustMintValue val -- the only constraint we add is: (the txn must mint the value of the Native_Token)
    ledgerTx <- submitTxConstraintsWith @Void lookups tx -- Submit the txn, the collateral is automatically added
        -- If amount is +ve, the minted Value will be automatically transferred to the wallet
        -- If amount is -ve, the tokens in the users wallet will be burned
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx -- wait for confirmation
    Contract.logInfo @String $ printf "forged %s" (show val) -- log Value

endpoints :: Contract () FreeSchema Text ()
endpoints = mint' >> endpoints -- Our only endpoint is mint, it recursively calls endpoint so we can call it again
  where
    mint' = awaitPromise $ endpoint @"mint" mint

-- =========== To test it in the playground
mkSchemaDefinitions ''FreeSchema

mkKnownCurrencies []

-- ========== To test it using EmulatorTrace
test :: IO ()
test = runEmulatorTraceIO $ do
    let tn = "ABC" -- TokenName
    -- Start wallets 1 & 2
    h1 <- activateContractWallet (knownWallet 1) endpoints
    h2 <- activateContractWallet (knownWallet 2) endpoints
    callEndpoint @"mint" h1 $ MintParams -- call mint on wallet 1
        { mpTokenName = tn
        , mpAmount    = 555 -- Wallet 1 mints 555 coins of that token
        }
    callEndpoint @"mint" h2 $ MintParams -- call mint on wallet 2
        { mpTokenName = tn
        , mpAmount    = 444
        }
    void $ Emulator.waitNSlots 1 -- wait one slot
    callEndpoint @"mint" h1 $ MintParams -- call mint (with -ve amount) on wallet 1
        { mpTokenName = tn
        , mpAmount    = -222 -- wallet 1 burns 222 coins of that token
        }
    void $ Emulator.waitNSlots 1 -- wait one slot

-- In the end, Wallet 1 should have 333 tokens and wallet 2 should have 444 tokens
-- The UTxOs containing the Native_Token will have some ADA in it, because every UTxO must have some min amount of ADA
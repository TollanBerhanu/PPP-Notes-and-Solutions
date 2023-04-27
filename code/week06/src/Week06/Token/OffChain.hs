{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Week06.Token.OffChain
    ( TokenParams (..)
    , adjustAndSubmit, adjustAndSubmitWith
    , mintToken
    ) where

import           Control.Monad               hiding (fmap)
import           Data.Aeson                  (FromJSON, ToJSON)
import qualified Data.Map                    as Map
import           Data.Maybe                  (fromJust)
import           Data.OpenApi.Schema         (ToSchema)
import           Data.Text                   (Text, pack)
import           Data.Void                   (Void)
import           GHC.Generics                (Generic)
import           Plutus.Contract             as Contract
import           Plutus.Contract.Wallet      (getUnspentOutput)
import qualified PlutusTx
import           PlutusTx.Prelude            hiding (Semigroup(..), unless)
import           Ledger                      hiding (mint, singleton)
import           Ledger.Constraints          as Constraints
import qualified Ledger.Typed.Scripts        as Scripts
import           Ledger.Value                as Value
import           Prelude                     (Semigroup (..), Show (..), String)
import qualified Prelude
import           Text.Printf                 (printf)

import           Week06.Token.OnChain
import           Week06.Utils                (getCredentials)

-- This module is used for minting native tokens
data TokenParams = TokenParams
    { tpToken   :: !TokenName   -- name of token to be minted
    , tpAmount  :: !Integer     -- amount to  be minted  
    , tpAddress :: !Address     -- address of the owner of the token
    } deriving (Prelude.Eq, Prelude.Ord, Generic, FromJSON, ToJSON, ToSchema, Show)

-- This helper function takes an unbalanced txn (a txn that has no min Ada set in its o/p), and balances and submits it
-- We use this after we create our 'unbalanced' txn below in the function 'mintToken'
adjustAndSubmitWith :: ( PlutusTx.FromData (Scripts.DatumType a)
                       , PlutusTx.ToData (Scripts.RedeemerType a)
                       , PlutusTx.ToData (Scripts.DatumType a)
                       , AsContractError e
                       )
                    => ScriptLookups a
                    -> TxConstraints (Scripts.RedeemerType a) (Scripts.DatumType a)
                    -> Contract w s e CardanoTx
adjustAndSubmitWith lookups constraints = do 
    unbalanced <- adjustUnbalancedTx <$> mkTxConstraints lookups constraints  -- 'mkTxConstraints' takes lookups and constraints to make an unbalanced txn
    Contract.logDebug @String $ printf "unbalanced: %s" $ show unbalanced       -- 'adjustUnbalancedTx' will add the min ADA to all the outputs
    unsigned <- balanceTx unbalanced                                          -- 'balanceTx' balances the txn 
    Contract.logDebug @String $ printf "balanced: %s" $ show unsigned
    signed <- submitBalancedTx unsigned                                       -- 'submitBalancedTx' signs and submits the balanced txn
    Contract.logDebug @String $ printf "signed: %s" $ show signed
    return signed                                                       -- All this could be done in one line, but we wanted to log all the intermediate outputs

-- This function does the same as the above but this one doesn't expect 'lookups'
adjustAndSubmit :: ( PlutusTx.FromData (Scripts.DatumType a)
                   , PlutusTx.ToData (Scripts.RedeemerType a)
                   , PlutusTx.ToData (Scripts.DatumType a)
                   , AsContractError e
                   )
                => Scripts.TypedValidator a
                -> TxConstraints (Scripts.RedeemerType a) (Scripts.DatumType a)
                -> Contract w s e CardanoTx
adjustAndSubmit inst = adjustAndSubmitWith $ Constraints.typedValidatorLookups inst

mintToken :: TokenParams -> Contract w s Text CurrencySymbol
mintToken tp = do
    Contract.logDebug @String $ printf "started minting: %s" $ show tp
    let addr = tpAddress tp
    case getCredentials addr of -- getCredentials is a helper function that extracts Maybe (Payment PubKeyHash, Maybe Staking PubKeyHash) from an address
        Nothing      -> Contract.throwError $ pack $ printf "expected pubkey address, but got %s" $ show addr
        Just (x, my) -> do
            oref <- getUnspentOutput    -- gets txOutRef from the wallet that's running this Contract Monad
            o    <- fromJust <$> Contract.txOutFromRef oref -- gets UTxO from txOutRef ... fromJust extracts the just from the maybe ... <$> is fmap
            Contract.logDebug @String $ printf "picked UTxO at %s with value %s" (show oref) (show $ _ciTxOutValue o)

            let tn          = tpToken tp
                amt         = tpAmount tp
                cs          = tokenCurSymbol oref tn amt    -- compute the CurrencySymbol
                val         = Value.singleton cs tn amt     -- compute the Value we want to mint
                c           = case my of
                    Nothing -> Constraints.mustPayToPubKey x val    -- we use 'mustPayToPubKey' when the address doesn't contain any Staking Credential
                    Just y  -> Constraints.mustPayToPubKeyAddress x y val   -- we use 'mustPayToPubKeyAddress' when Staking Credential exists in the address
                lookups     = Constraints.mintingPolicy (tokenPolicy oref tn amt) <>    -- Specify the minting policy by serializing it using the helper func tokenPolicy
                              Constraints.unspentOutputs (Map.singleton oref o)         -- Specify one UTxO using Map.singleton which maps one key to one value .. singleton :: k -> a -> Map k a
                constraints = Constraints.mustMintValue val          <>             -- Specify the value we want to mint
                              Constraints.mustSpendPubKeyOutput oref <>             -- Specify the UTxO we want to spend
                              c                                                     -- Specify the constraint defined above (which pubKey we send to)

            void $ adjustAndSubmitWith @Void lookups constraints
            Contract.logInfo @String $ printf "minted %s" (show val)
            return cs

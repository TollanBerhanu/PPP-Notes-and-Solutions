{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Week02.Typed where

import           Control.Monad        hiding (fmap)
import           Data.Map             as Map
import           Data.Text            (Text)
import           Data.Void            (Void)
import           Plutus.Contract
import           PlutusTx             (Data (..))
import qualified PlutusTx
import qualified PlutusTx.Builtins    as Builtins
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (IO, Semigroup (..), String)
import           Text.Printf          (printf)

{-# INLINABLE mkValidator #-}
mkValidator :: () -> Integer -> ScriptContext -> Bool
            -- Datum -> Redeemer -> Context -> ()
mkValidator _ r _ = traceIfFalse "wrong redeemer" $ r == 42

-- This is mostly boilerplate ... using high-level datatypes require more boilerplate and more resources
-- ******************************************************************************
data Typed
instance Scripts.ValidatorTypes Typed where
    type instance DatumType Typed = () -- tell plutus the datatype of datum is ()
    type instance RedeemerType Typed = Integer -- and the datatype of the redeemer is Integer

typedValidator :: Scripts.TypedValidator Typed
typedValidator = Scripts.mkTypedValidator @Typed -- typed version of validator
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||]) 
  where
    wrap = Scripts.wrapValidator @() @Integer -- wrap translates from the high-level type to low level
                                              -- it uses toData and fromData functions to convert to/from BuiltInData
    -- For example: toData () ... Constr 0 []                               toData (42 :: Integer) ... I 42
    --              fromData (Constr 0 []) :: Maybe () ... Just ()          fromData (I 42) :: Maybe Integer ... Just 42


validator :: Validator
validator = Scripts.validatorScript typedValidator -- convert the typed validator to an untyped validator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator -- create a validator using typed validator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator -- create a script address using an untyped validator
-- ******************************************************************************
 
-- Here starts the off-chain code
type GiftSchema =
            Endpoint "give" Integer
        .\/ Endpoint "grab" Integer

give :: AsContractError e => Integer -> Contract w s e ()
give amount = do
    let tx = mustPayToTheScript () $ Ada.lovelaceValueOf amount -- mustPayToTheScript means that the txn only involves one
    -- script, even though the txn can take i/ps / produce o/ps from different script addresses, you can define one script
    -- as 'the script'. Its a common case in typed version than mustPayToOtherScript because we directly give it the datum()
    ledgerTx <- submitTxConstraints typedValidator tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "made a gift of %d lovelace" amount

grab :: forall w s e. AsContractError e => Integer -> Contract w s e ()
grab r = do
    utxos <- utxosAt scrAddress
    let orefs   = fst <$> Map.toList utxos
        lookups = Constraints.unspentOutputs utxos      <>
                  Constraints.otherScript validator
        tx :: TxConstraints Void Void
        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ Builtins.mkI r | oref <- orefs]
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ "collected gifts"

endpoints :: Contract () GiftSchema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" give
    grab' = endpoint @"grab" grab

mkSchemaDefinitions ''GiftSchema

mkKnownCurrencies []

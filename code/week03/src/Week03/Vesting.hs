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

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Week03.Vesting where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.Map             as Map
import           Data.Text            (Text)
import           Data.Void            (Void)
import           GHC.Generics         (Generic)
import           Plutus.Contract
import           PlutusTx             (Data (..))
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   (TxConstraints)
import qualified Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (IO, Semigroup (..), Show (..), String)
import           Text.Printf          (printf)

-- In this script, a beneficiary can access funds (UTxOs at the vesting script address) after a certain deadline
-- has passed (then that person can use the o/p (UTxO sitting at the vesting address as an i/p to his/her txns))

-- The beneficiary is specified by the person that sends the UTxO to the vesting address. The beneficiary then
-- checks whether he/she is listed and can pick and grab/consume a UTxO with his/her public key. 

data VestingDatum = VestingDatum
    { beneficiary :: PaymentPubKeyHash -- the beneficiary will be identified by their public key
    , deadline    :: POSIXTime  -- we use real time for the deadline
    } deriving Show

PlutusTx.unstableMakeIsData ''VestingDatum -- use template haskell to automatically convert custom type to BuiltinData 

{-# INLINABLE mkValidator #-}
mkValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkValidator dat () ctx = traceIfFalse "beneficiary's signature missing" signedByBeneficiary &&
                         traceIfFalse "deadline not reached" deadlineReached
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx -- data ScriptContext = ScriptContext {scriptContextTxInfo :: TxInfo, ...}
                                        -- Defined in ‘Plutus.V1.Ledger.Contexts’
    signedByBeneficiary :: Bool
    signedByBeneficiary = txSignedBy info $ unPaymentPubKeyHash $ beneficiary dat -- txSignedBy :: TxInfo -> PubKeyHash -> Bool
            --    newtype PaymentPubKeyHash = PaymentPubKeyHash {unPaymentPubKeyHash :: PubKeyHash}     -- Defined in ‘Ledger.Address’

    deadlineReached :: Bool
    deadlineReached = contains (from $ deadline dat) $ txInfoValidRange info
    -- the whole validity interval of the txn must be after the deadline to know deterministicly that the current time has reached
    -- the deadline and for the txn to be valid. This means if (deadline -> eternity) `contains` (valid_range)  

data Vesting
instance Scripts.ValidatorTypes Vesting where
    type instance DatumType Vesting = VestingDatum
    type instance RedeemerType Vesting = ()

typedValidator :: Scripts.TypedValidator Vesting
typedValidator = Scripts.mkTypedValidator @Vesting
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @VestingDatum @()

validator :: Validator
validator = Scripts.validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

-- ====================================== OFF-CHAIN CODE ======================================
data GiveParams = GiveParams
    { gpBeneficiary :: !PaymentPubKeyHash   -- public key of beneficiary to give to
    , gpDeadline    :: !POSIXTime           -- deadline unlock the UTxO
    , gpAmount      :: !Integer             -- amount of Ada the UTxO is worth
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type VestingSchema =
            Endpoint "give" GiveParams
        .\/ Endpoint "grab" ()  -- grab needs no parameters ... you can just grab if you are allowed to

give :: AsContractError e => GiveParams -> Contract w s e ()
give gp = do
    let dat = VestingDatum -- include the data about the beneficiary and the deadline on the datum
                { beneficiary = gpBeneficiary gp
                , deadline    = gpDeadline gp
                }
        tx  = Constraints.mustPayToTheScript dat $ Ada.lovelaceValueOf $ gpAmount gp -- create a txn that an o/p at this script address
    ledgerTx <- submitTxConstraints typedValidator tx --submit the txn                -- and include the datum and value of lovelace
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx -- wait for confirmation using the the TxnId 
    logInfo @String $ printf "made a gift of %d lovelace to %s with deadline %s"
        (gpAmount gp)
        (show $ gpBeneficiary gp)
        (show $ gpDeadline gp)

grab :: forall w s e. AsContractError e => Contract w s e ()
grab = do
    now   <- currentTime -- get the current time (to compare with the deadline)
    pkh   <- ownPaymentPubKeyHash -- get my own public key address hash
    utxos <- Map.filter (isSuitable pkh now) <$> utxosAt scrAddress -- get all the UTxOs sitting at the script address and filter the
    if Map.null utxos                                               -- ones that are suitable (same pubKeyHash and passed deadline)
        then logInfo @String $ "no gifts available"
        else do -- create a txn that collects all the suitable UTxOs (might be too many (more than the valid txn size limit) in the
            let orefs   = fst <$> Map.toList utxos                          --  real world, but we ignored this case for simplicity)
                    -- grab all the references to the UTxOs we want to collect 
                lookups = Constraints.unspentOutputs utxos  <> -- create a lookup containing all teh UTxOs and 
                          Constraints.otherScript validator    -- the validator script
                tx :: TxConstraints Void Void
                tx      = mconcat [Constraints.mustSpendScriptOutput oref unitRedeemer | oref <- orefs] <> -- add a constraint where
                                                -- we loop over all the UTxOs and make them script outputs (spend each of the UTxOs)
                          Constraints.mustValidateIn (from now) -- set the time interval to be later than now (to be sure that the       
            ledgerTx <- submitTxConstraintsWith @Void lookups tx                                           -- deadline has passed)
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            logInfo @String $ "collected gifts"
  where                                             -- all UTxOs at the script addr
    isSuitable :: PaymentPubKeyHash -> POSIXTime -> ChainIndexTxOut -> Bool
    isSuitable pkh now o = case _ciTxOutDatum o of -- _ciTxOutDatum ... checks the datum of the o/p (Left _ if the datum is a hash)
                                                   --  (right _ if the actual serialized datum is contained in the o/p of the txn)
        Left _          -> False -- drop the UTxO if datum is just a hash (can't extract info out of that)
        Right (Datum e) -> case PlutusTx.fromBuiltinData e of -- try to deserialize the datum back to our custom type (VestingDatum)
            Nothing -> False -- invalid datum
            Just d  -> beneficiary d == pkh && deadline d <= now -- check that I am the beneficiary & that the deadline has passed

endpoints :: Contract () VestingSchema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints -- recursively call endpoints (give/grab)
  where
    give' = endpoint @"give" give
    grab' = endpoint @"grab" $ const grab

mkSchemaDefinitions ''VestingSchema

mkKnownCurrencies []

-- When we try this in the playground, we need wallet addresses. We can get some mock wallet addresses like this:
{-
Prelude Wallet.Emulator> mockWalletPaymentPubKeyHash $ knownWallet 2
80a4f45b56b88d1139da23bc4c3c75ec6d32943c087f250b86193ca7

Prelude Wallet.Emulator> mockWalletPaymentPubKeyHash $ knownWallet 3
2e0ad60c3207248cecd47dbde3d752e0aad141d6b8f81ac2c6eca27c 

** knownWallet (1 - 10) are mock wallets that also exist in plutus playground
-}

-- Next we need to convert 10 and 20 slots to POSIX time to use them in the playground.
{-
Prelude Ledger.TimeSlot Data.Default> slotToBeginPOSIXTime def 10
POSIXTime {getPOSIXTime = 1596059101000}

Prelude Ledger.TimeSlot Data.Default> slotToBeginPOSIXTime def 20
POSIXTime {getPOSIXTime = 1596059111000}

** Plutus uses POSIX time while wallet applications like dedalus uses Slots to keep track of time
-}

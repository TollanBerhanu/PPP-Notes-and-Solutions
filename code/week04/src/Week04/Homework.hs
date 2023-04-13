{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NumericUnderscores #-} -- Allow a readable format of writing numbers (10000000 ... 10_000_000)
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}

module Week04.Homework where

import Data.Aeson            (FromJSON, ToJSON)
import Data.Functor          (void)
import Data.Text             (Text)
import GHC.Generics          (Generic)
import Ledger
import Ledger.Ada            as Ada
import Ledger.Constraints    as Constraints
import Plutus.Contract       as Contract
import Plutus.Trace.Emulator as Emulator

data PayParams = PayParams
    { ppRecipient :: PaymentPubKeyHash
    , ppLovelace  :: Integer
    } deriving (Show, Generic, FromJSON, ToJSON)

type PaySchema = Endpoint "pay" PayParams

payContract :: Contract () PaySchema Text ()
payContract = do
    pp <- awaitPromise $ endpoint @"pay" return -- return takes a parameter type of PayParams and produces a Contract with no side-effects
        -- that immediately returns the PayParams value, then we bind this value to pp. We pass the PayParams in the EmulatorTrace.
    -- Now we have out PayParams, lets construct the txn
    let tx = mustPayToPubKey (ppRecipient pp) $ lovelaceValueOf $ ppLovelace pp -- mustPayToPubKey :: PubKey(Recipient) -> Amount(lovelace)
                                                -- lovelaceValueOf :: Integer -> lovelace
    void $ submitTx tx -- takes descriptions of a txn and created an actual txn
                       -- it automatically takes the available UTxOs from sender's wallet, and creates a change o/p. Finally, it
                       -- submits the txn to the blockchain
    payContract -- recursively call the contract if you want to make another payment
    
-- ######### Homework Task 2
payContractHandler :: Contract () PaySchema Void ()
payContractHandler = Contract.handleError
    (\err -> Contract.logError $ "Caught Error: " ++ unpack err) 
    payContract

-- ######### Homework Task 1
-- A trace that invokes the pay endpoint of payContract on Wallet 1 twice, each time with Wallet 2 as
-- recipient, but with amounts given by the two arguments. There should be a delay of one slot
-- after each endpoint call.
payTrace :: Integer -> Integer -> EmulatorTrace ()
payTrace pay1 pay2 = do
    h <- activateContractWallet (knownWallet 1) payContractHandler
    let pk2 = mockWalletPaymentPubKeyHash $ knownWallet 2

    callEndpoint @"pay" h (PayParams pk2 pay1)
    void $ Emulator.waitNSlots 1
    callEndpoint @"pay" h (PayParams pk2 pay2) 
    void $ Emulator.waitNSlots 1


payTest1 :: IO ()
payTest1 = runEmulatorTraceIO $ payTrace 10_000_000 20_000_000

payTest2 :: IO ()
payTest2 = runEmulatorTraceIO $ payTrace 1000_000_000 20_000_000 
    -- should throw an error because knownWallet1 has initial funds of only 100 ADA 

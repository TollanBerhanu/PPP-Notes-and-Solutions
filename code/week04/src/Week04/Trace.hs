{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds        #-}

module Week04.Trace where

import Control.Monad.Freer.Extras as Extras
import Data.Default               (Default (..))
import Data.Functor               (void)
import Ledger.TimeSlot
import Plutus.Trace
import Wallet.Emulator.Wallet

import Week04.Vesting

-- Contract w s e a
-- EmulatorTrace a

test :: IO ()
test = runEmulatorTraceIO myTrace -- Runs the emulator trace an displays the result in the console

myTrace :: EmulatorTrace () -- Emulator trace is an instance of monad, so we can use the do notation to bind actions sequentially(>>=)
myTrace = do
    h1 <- activateContractWallet (knownWallet 1) endpoints -- unlike the plutus playground, we have to manually start the contracts 
    h2 <- activateContractWallet (knownWallet 2) endpoints -- themselves before just calling the endpoints,... using activateContractWallet
            -- we give it the wallet (on which to activate this contract) and the contract.. in this case the wallet is a default wallet
            -- the result we get from this is called a handle
    callEndpoint @"give" h1 $ GiveParams
        { gpBeneficiary = mockWalletPaymentPubKeyHash $ knownWallet 2
        , gpDeadline    = slotToBeginPOSIXTime def 20
        , gpAmount      = 10000000
        }
    void $ waitUntilSlot 20
    callEndpoint @"grab" h2 ()
    s <- waitNSlots 2
    Extras.logInfo $ "reached " ++ show s

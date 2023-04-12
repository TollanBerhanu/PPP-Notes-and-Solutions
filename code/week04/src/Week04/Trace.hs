-- The EmulatorTrace monad allows to test our on-chain code without using the plutus playground. 

{-# LANGUAGE TypeApplications #-} -- Used for the @ symbol, used after the 'callEndpoint' function
                                  -- @"name-of-endpoint" = type of endpoint ... e.g., give, grab
{-# LANGUAGE DataKinds        #-}

module Week04.Trace where

import Control.Monad.Freer.Extras as Extras -- to log from the emulator trace monad
import Data.Default               (Default (..)) -- import the default configurations
import Data.Functor               (void) -- Used for waitUntilSlot below to ignore the result of the inner function and return unit
import Ledger.TimeSlot
import Plutus.Trace
import Wallet.Emulator.Wallet

import Week04.Vesting -- we import the "give" and "grab" endpoints

-- Contract w s e a
-- EmulatorTrace a

test :: IO ()
test = runEmulatorTraceIO myTrace -- Runs the emulator trace an displays the log in the console

myTrace :: EmulatorTrace () -- Emulator trace is an instance of monad, so we can use the do notation to bind actions sequentially(>>=)
myTrace = do
    h1 <- activateContractWallet (knownWallet 1) endpoints -- unlike the plutus playground, we have to manually start the contracts 
    h2 <- activateContractWallet (knownWallet 2) endpoints -- themselves before just calling the endpoints,... using activateContractWallet
            -- we give it the wallet (on which to activate this contract) and the contract.. in this case the wallet is a default wallet
            -- the result we get from this is called a 'contract handle'... it is used to later reference this contract
            -- Both wallets run the 'endpoints' contract

    callEndpoint @"give" h1 $ GiveParams -- callEndpoint :: typeOfEndpoint -> handler -> paramsThatEndpointTakes (GiveParams {..})
        { gpBeneficiary = mockWalletPaymentPubKeyHash $ knownWallet 2 -- pubKeyHash of wallet 2
        , gpDeadline    = slotToBeginPOSIXTime def 20 -- wait for 20 slots (def is the default configutaion)
        , gpAmount      = 10000000 -- 10 ADA
        }
    void $ waitUntilSlot 20 -- wait until the deadline has passed (as defined above)
                            -- we use void to ignore the return value of waitUntilSlot which has type Slot { getSlot = 20}, had we not 
                            -- done this, the compiler will issue a warning that says you are ignoring a result of type slot
    callEndpoint @"grab" h2 () -- the second wallet (using the second handle) will call the "grab" endpoint with no parameters
    s <- waitNSlots 2 -- wait 2 slots for the txn to be processed
    Extras.logInfo $ "reached " ++ show s -- log the result of waitNSlots from inside the on-chain code (i.e., the Emulator monad)

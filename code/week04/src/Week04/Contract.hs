{-# LANGUAGE OverloadedStrings #-} -- allows you to use literal strings for more general types like String, Text or Bytestring
{-# LANGUAGE TypeApplications  #-} -- this will disambiguate the effect of overloaded strings by using @Type "..."
                                   -- it is specifically used for polymorphic functions (functions that take arbitrary types
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}

module Week04.Contract where

import Control.Monad.Freer.Extras as Extras
import Data.Functor               (void)
import Data.Text                  (Text, unpack)
import Data.Void                  (Void) -- is a type that has no values, unlike unit that has one value, unit. () :: ()
import Plutus.Contract            as Contract
import Plutus.Trace.Emulator      as Emulator
import Wallet.Emulator.Wallet

-- Contract w s e a
-- w :- allows a contract to write log messages of type w to allow it to communicate with other contracts
-- s :- specifie shte endpoints in the contract
-- e :- error message. It's more general because you can specify the type of the error message
      -- allows you to throw exceptions and catch them inside the contract monad

-- EmulatorTrace a

-- We don't want any log messages and no endpoints(Empty). The error message is of type Text (large string). The result is unit
myContract1 :: Contract () Empty Text () 
myContract1 = do
    void $ Contract.throwError "BOOM!" -- we add Contract. because we also have a throwError function in the EmulatorTrace monad
        -- this is used to throw errors coming from the Plutus contract while the other is for errors coming from the Plutus emulator
        -- the void is used to avaid a warning that says you discarded a result of type GHC.Types.Any
    Contract.logInfo @String "hello from the contract" -- we use @String because of the OverloadedStrings pragma
                                                       -- we are telling the compiler that the literal string has a 'String' type
        -- Logged inside the  off-chain code (the Contract monad)
        -- This log message won't be shown as long as the exception "BOOM!" is thrown before it 
    
myTrace1 :: EmulatorTrace ()
myTrace1 = void $ activateContractWallet (knownWallet 1) myContract1 -- to start/activate the contract we defined above 
            -- activateContractWallet returns a handle ... we use void to ignore its retval and return unit

test1 :: IO ()
test1 = runEmulatorTraceIO myTrace1 -- run the emulator trace and log its results


-- ================ myContract2 is the same as myContract1 with exception handling =====================
myContract2 :: Contract () Empty Void () -- we make the third type param (e) void, so that the caontract can't throw exceptions
                                                                               -- (you can't give it an error msg of type void)
myContract2 = Contract.handleError -- handleError takes a handler (E -> Contract _ _ E' -> a) and a contract (Contract _ _ E -> a)
                                   -- the overall function has result of type a
    (\err -> Contract.logError $ "caught: " ++ unpack err)
    myContract1
    -- first the contract (second argument) is executed, if there os no exception, its result will be the result of the overall contract
    -- if there is an exception, we apply out handler to it 

myTrace2 :: EmulatorTrace ()
myTrace2 = void $ activateContractWallet (knownWallet 1) myContract2

test2 :: IO ()
test2 = runEmulatorTraceIO myTrace2

type MySchema = Endpoint "foo" Int .\/ Endpoint "bar" String

myContract3 :: Contract () MySchema Text ()
myContract3 = do
    awaitPromise $ endpoint @"foo" Contract.logInfo
    awaitPromise $ endpoint @"bar" Contract.logInfo

myTrace3 :: EmulatorTrace ()
myTrace3 = do
    h <- activateContractWallet (knownWallet 1) myContract3
    callEndpoint @"foo" h 42
    callEndpoint @"bar" h "Haskell"

test3 :: IO ()
test3 = runEmulatorTraceIO myTrace3

myContract4 :: Contract [Int] Empty Text ()
myContract4 = do
    void $ Contract.waitNSlots 10
    tell [1]
    void $ Contract.waitNSlots 10
    tell [2]
    void $ Contract.waitNSlots 10

myTrace4 :: EmulatorTrace ()
myTrace4 = do
    h <- activateContractWallet (knownWallet 1) myContract4

    void $ Emulator.waitNSlots 5
    xs <- observableState h
    Extras.logInfo $ show xs

    void $ Emulator.waitNSlots 10
    ys <- observableState h
    Extras.logInfo $ show ys

    void $ Emulator.waitNSlots 10
    zs <- observableState h
    Extras.logInfo $ show zs

test4 :: IO ()
test4 = runEmulatorTraceIO myTrace4

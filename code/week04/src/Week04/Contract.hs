{-# LANGUAGE OverloadedStrings #-} -- allows you to use literal strings for more general types like String, Text or Bytestring
{-# LANGUAGE TypeApplications  #-} -- this will disambiguate the effect of overloaded strings by using @Type "..."
                                   -- it is specifically used for polymorphic functions (functions that take arbitrary types
{-# LANGUAGE DataKinds         #-} -- enable type level strings to define endpoints (e.g., "foo", "give", "grab")
{-# LANGUAGE TypeOperators     #-} -- Used for the type operator .\/ when we define multiple endpoints

module Week04.Contract where

import Control.Monad.Freer.Extras as Extras
import Data.Functor               (void)
import Data.Text                  (Text, unpack)
import Data.Void                  (Void) -- is a type that has no values, unlike unit that has one value, unit. () :: ()
import Plutus.Contract            as Contract
import Plutus.Trace.Emulator      as Emulator -- callEndpoint is defined here
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


-- ================ myContract2 adds exception handling on myContract1 =====================
myContract2 :: Contract () Empty Void () -- we make the third type param (e) void, so that the caontract can't throw exceptions
                                                                               -- (you can't give it an error msg of type void)
myContract2 = Contract.handleError -- handleError takes a handler (E -> Contract _ _ E' -> a) and a contract (Contract _ _ E -> a)
                                   -- the overall function has result of type a
    (\err -> Contract.logError $ "caught: " ++ unpack err) -- err has type Text, so we use unpack to convert it to String
                                -- "caught:" is now a string because it's concatenated with a String (no need for @String)
    myContract1
    -- first, the contract (2nd argument) is executed, if there is no exception, its result will be the result of the overall contract
    -- but if there is an exception, we apply our handler to it (E) and run the contract we got back (Contract _ _ E' -> a)

myTrace2 :: EmulatorTrace ()
myTrace2 = void $ activateContractWallet (knownWallet 1) myContract2

test2 :: IO ()
test2 = runEmulatorTraceIO myTrace2
    -- Running test2 will log out "caught: BOOM!" because myContract1 throws and error and myContract2 catches it and logs it out 

-- Now lets look at the second argument of (Cotract w s e a) ... the schema. This is used to define endpoints by which the user can
-- interact with the contract (e.g., in the dapp)
type MySchema = Endpoint "foo" Int .\/ Endpoint "bar" String -- defiing the schema (endpoints the contract should have)
                    -- here "foo" is a type-level string (not a value-level string)
                    -- Endpoint "foo" Int: creates an endpoint "foo" that takes an Int and a (Contract ..) and returns a (Promise ..)
                    -- We can define multiple endpoints by separating them with the type-operator .\/
myContract3 :: Contract () MySchema Text ()
myContract3 = do
    awaitPromise $ endpoint @"foo" Contract.logInfo -- the endpoint "foo" takes a function of type (Int -> Contract)
    awaitPromise $ endpoint @"bar" Contract.logInfo     -- Contract.logInfo takes any type that's serilizable to json (e.g., Int)
                                                        -- and gives us a function that logs that Int
    -- awaitPromise takes a promise and returns a Contract
    -- a (Promise) is a (Contract) triggered by an outside stimulus... until the endpoint is called with the required param (Int/String)

myTrace3 :: EmulatorTrace ()
myTrace3 = do
    h <- activateContractWallet (knownWallet 1) myContract3 -- we need the handle in this case, so we don't use void
    callEndpoint @"foo" h 42 -- the endpoint "foo" expects an Int
    callEndpoint @"bar" h "Haskell" -- the endpoint "bar" expects a String
    -- to call the endpoint from the EmulatorTrace, so that it doesn't keep waiting until smn does it manually
    -- callEndpoint takes the endpoint, the handle and a value(param) to apply to the endpoint

test3 :: IO ()
test3 = runEmulatorTraceIO myTrace3

-- Now we'll see how we can use state (the first argument). The main purpose of using state is for the contract to communicate info 
-- to the outside. In this case, the outside is the EmulatorTrace, but in reality it can be the user (browser)
                    -- the state 'w' must be a Monoid instance, initial state is mempty of the Monoid
myContract4 :: Contract [Int] Empty Text ()
myContract4 = do
    void $ Contract.waitNSlots 10 -- wait 10 slots... duh, also we don't need the retval Slot {getSlot=10}, so we use void 
    tell [1] -- tell :: [a] -> Writer () ... takes a log and returns a unit Writer with that log (Writer () [a])
    void $ Contract.waitNSlots 10
    tell [2] -- each time we call tell, mappend is used to update the state (prevState `mappend` tell[currState])
    void $ Contract.waitNSlots 10

myTrace4 :: EmulatorTrace ()
myTrace4 = do
    h <- activateContractWallet (knownWallet 1) myContract4

    void $ Emulator.waitNSlots 5
    xs <- observableState h -- lookup the state of the running contract at this point in time (after 5 slots)
                            -- it takes the handle of the running contract whose state we want to observe and returns the state
    Extras.logInfo $ show xs -- log out the state at that point
    -- No state will appear here because the first state is added after 10 slots, as defined above
    -- No state means [] ... mempty [Int]

    void $ Emulator.waitNSlots 10
    ys <- observableState h
    Extras.logInfo $ show ys
    -- [1] is logged here ... prevState `mappend` [1]

    void $ Emulator.waitNSlots 10
    zs <- observableState h
    Extras.logInfo $ show zs
    -- [1,2] is logged here ... prevState `mappend` [2]

test4 :: IO ()
test4 = runEmulatorTraceIO myTrace4


module State where

import           Control.Monad.State (State, get, put, runState)

---------------------------------------------------------------------------------------------------
--------------------------------- HELPER FUNCTIONS/TYPES ------------------------------------------

-- Mock UTxO type
data UTxO = UTxO { owner :: String , value :: Integer }
    deriving (Show, Eq)

-- Mock blockchain type (just a list of utxos)
newtype Mock = Mock { utxos :: [UTxO] }
    deriving (Show, Eq)

-- Initial blockchain state
initialMockS :: Mock
initialMockS = Mock [ UTxO "Alice" 1000 ]

---------------------------------------------------------------------------------------------------
------------------------------------ WITHOUT STATE MONAD ------------------------------------------

-- This function is used to transfer money from one person to another
-- senders_name -> amount_sent -> receiver's_name -> current_state_of_blockchain  ->  (Succeed/fail, new_state)
sendValue :: String -> Integer -> String -> Mock -> (Bool, Mock)
sendValue from amount to mockS =
    let senderUtxos = filter ((== from) . owner) (utxos mockS)  -- filter all the UTxOs of the sender from the current state of the blockchain
        blockchainWithoutSenderUtxos = filter ((/= from) . owner) (utxos mockS) -- filter all UTxOs that are not owned by the sender so that we can put them back
                                        -- into the new state if the txn succeeds (all the sender's UTxOs will be consumed and one change UTxO will be sent back)
        totalSenderFunds = sum (map value senderUtxos) -- add up all the values of the sender's UTxOs
        receiverUtxo = UTxO to amount   -- create a UTxO that is given to the receiver
        senderChange = UTxO from (totalSenderFunds - amount)    -- create a change UTxO for the sender
    in if totalSenderFunds >= amount    -- this only happens if the sender has enough funds
        then (True, Mock $ [receiverUtxo] ++ [senderChange] ++ blockchainWithoutSenderUtxos)    -- Txn succeeds, create new state with all the appropriate UTxOs
        else (False, mockS)     -- Txn fails, return the old state

-- This functions tests multiple txns and returns the final state of the blockchain and a bool that indicates whether all the txns succeed or not
multipleTx :: (Bool, Mock)
multipleTx =
    let (isOk,  mockS1) = sendValue "Alice" 100 "Bob"   initialMockS    -- (True, [UTxO "Alice" 900, UTxO "Bob" 100])
        (isOk2, mockS2) = sendValue "Alice" 300 "Bob"   mockS1          -- (True, [UTxO "Alice" 600, UTxO "Bob" 400])
        (isOk3, mockS3) = sendValue "Bob"   200 "Rick"  mockS2          -- (True, [UTxO "Alice" 600, UTxO "Bob" 200, UTxO "Rick" 200])
    in (isOk && isOk2 && isOk3, mockS3)

---------------------------------------------------------------------------------------------------
-------------------------------------- WITH STATE MONAD -------------------------------------------
    -- (State state_type ret_val_type) ... Mock -> (Bool, Mock)
-- newtype State s a = State { runState :: s    -> (a   , s) }

--          From -> Amount -> To (Notice we don't provide the initial state here)
sendValue' :: String -> Integer -> String -> State Mock Bool
sendValue' from amount to = do
    mockS <- get                    -- get :: MonadState s m => m s ... get the current state to extract UTxOs from it
        -- we first have to get the state of the blockchain... get returns the state from the internals of a monad
    let senderUtxos = filter ((== from) . owner) (utxos mockS)
        blockchainWithoutSenderUtxos = filter ((/= from) . owner) (utxos mockS)
        totalSenderFunds = sum (map value senderUtxos)
        receiverUtxo = UTxO to amount
        senderChange = UTxO from (totalSenderFunds - amount)
    if totalSenderFunds >= amount
        then do
            put $ Mock $ [receiverUtxo] ++ [senderChange] ++ blockchainWithoutSenderUtxos   -- put :: MonadState s m => s -> m () ... change the state of the blockchain
            return True     -- return the final value                                       -- put takes a state and replaces the state inside the monad
        else return False   -- the state is handled by the state monad


multipleTx' :: (Bool, Mock)
multipleTx' = runState (do          -- runState :: State s a -> s -> (a, s) ... runState :: (State computation) -> (initial state) -> (ret_val, final_state)
                                    -- it unwraps a state monad computation as a function (opposite of State)
    isOk  <- sendValue' "Alice" 100 "Bob"
    isOk2 <- sendValue' "Alice" 300 "Bob"
    isOk3 <- sendValue' "Bob"   200 "Rick"
    return (isOk && isOk2 && isOk3))
    initialMockS
-- (True,Mock {utxos = [UTxO {owner = "Rick", value = 200},UTxO {owner = "Bob", value = 200},UTxO {owner = "Alice", value = 600}]})

type Run a = State Mock a

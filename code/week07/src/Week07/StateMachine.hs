{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Week07.StateMachine
    ( Game (..)
    , GameChoice (..)
    , FirstParams (..)
    , SecondParams (..)
    , GameSchema
    , Last (..)
    , ThreadToken
    , Text
    , endpoints
    ) where

import           Control.Monad                hiding (fmap)
import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Monoid                  (Last (..))
import           Data.Text                    (Text, pack)
import           GHC.Generics                 (Generic)
import           Ledger                       hiding (singleton)
import           Ledger.Ada                   as Ada
import           Ledger.Constraints           as Constraints
import           Ledger.Typed.Tx
import qualified Ledger.Typed.Scripts         as Scripts
import           Plutus.Contract              as Contract
import           Plutus.Contract.StateMachine
import qualified PlutusTx
import           PlutusTx.Prelude             hiding (Semigroup(..), check, unless)
import           Playground.Contract          (ToSchema)
import           Prelude                      (Semigroup (..), Show (..), String)
import qualified Prelude

data Game = Game
    { gFirst          :: !PaymentPubKeyHash
    , gSecond         :: !PaymentPubKeyHash
    , gStake          :: !Integer
    , gPlayDeadline   :: !POSIXTime
    , gRevealDeadline :: !POSIXTime
    , gToken          :: !ThreadToken       -- The token has type of ThreadToken instead of AssetClass
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)

PlutusTx.makeLift ''Game

data GameChoice = Zero | One
    deriving (Show, Generic, FromJSON, ToJSON, ToSchema, Prelude.Eq, Prelude.Ord)

instance Eq GameChoice where
    {-# INLINABLE (==) #-}
    Zero == Zero = True
    One  == One  = True
    _    == _    = False

PlutusTx.unstableMakeIsData ''GameChoice

data GameDatum = GameDatum BuiltinByteString (Maybe GameChoice) | Finished  -- We have an additional value constructor 'Finished' to represent the final state of 
deriving Show                                                    -- our StateMachine. We don't put this in the UTxO, we just need it for our StateMachine to work

instance Eq GameDatum where
    {-# INLINABLE (==) #-}
    GameDatum bs mc == GameDatum bs' mc' = (bs == bs') && (mc == mc')
    Finished        == Finished          = True                 -- This doen't matter, we just had to represent all value constructors
    _               == _                 = False

PlutusTx.unstableMakeIsData ''GameDatum

data GameRedeemer = Play GameChoice | Reveal BuiltinByteString | ClaimFirst | ClaimSecond
    deriving Show

PlutusTx.unstableMakeIsData ''GameRedeemer

{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

{-# INLINABLE gameDatum #-}
gameDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe GameDatum
gameDatum o f = do
    dh      <- txOutDatum o
    Datum d <- f dh
    PlutusTx.fromBuiltinData d

-- The state machine will be represented by a UTxO sitting at the state machine script address and the state of the state machine will be the datum of that UTxO
-- A transition will be a txn that consumes the current state (UTxO containing state datum) using a redeemer that characterizes the transition (Play/reveal/..)
    -- and a new UTxO will be produced at address with the datum reflecting the new state

{-# INLINABLE transition #-}    -- all the validation checks are done in this transition function (this checks whether the combination of the datum, redeemer and  
transition :: Game -> State GameDatum -> GameRedeemer -> Maybe (TxConstraints Void Void, State GameDatum)                       -- txn constraints is valid or not)
transition game s r = case (stateValue s, stateData s, r) of   -- (Extract the (Value/stake of the UTxO we're consuming) and the (actual state) from 'State GameDatum')
    (v, GameDatum bs Nothing, Play c)       -- 1st player moves and the 2nd player is currently moving
        | lovelaces v == gStake game         -> Just ( Constraints.mustBeSignedBy (gSecond game)                    <> -- We usually use Constraints.. in off-chain code
                                                       Constraints.mustValidateIn (to $ gPlayDeadline game)
                                                     , State (GameDatum bs $ Just c) (lovelaceValueOf $ 2 * gStake game) -- This is the new state of the o/p UTxO
                                                     )
    (v, GameDatum _ (Just _), Reveal _)     -- 2nd player moves, 1st player wins (reveal nonce and collect winnings)
        | lovelaces v == (2 * gStake game)   -> Just ( Constraints.mustBeSignedBy (gFirst game)                     <> 
                                                       Constraints.mustValidateIn (to $ gRevealDeadline game)
                                                     , State Finished mempty    -- The new state is the finished/final state, no value is left at the contract address
                                                        -- A new UTxO will not be created here, the StateMachine just stops and the ThreadToken is burned
                                                     )
            -- checking the nonce can't be expressed interms of Constraints, so we leave it and add that condition when we define our StateMachine below
    (v, GameDatum _ Nothing, ClaimFirst)    -- 2nd player doesn't move, play deadline passes, 1st player gets back stake
        | lovelaces v == gStake game         -> Just ( Constraints.mustBeSignedBy (gFirst game)                     <>
                                                       Constraints.mustValidateIn (from $ 1 + gPlayDeadline game)
                                                     , State Finished mempty
                                                     )
    (v, GameDatum _ (Just _), ClaimSecond)  -- 2nd player moves, reveal deadline passes, 2nd player wins (collect winnings)
        | lovelaces v == (2 * gStake game)   -> Just ( Constraints.mustBeSignedBy (gSecond game)                    <>
                                                       Constraints.mustValidateIn (from $ 1 + gRevealDeadline game)
                                                     , State Finished mempty
                                                     )
    _                                        -> Nothing         -- We return Nothing if the transition is not allowed


{-# INLINABLE final #-}
final :: GameDatum -> Bool  -- Determine whether it's a final state or not based on the datum
final Finished = True
final _        = False

{-# INLINABLE check #-} -- Check if the hash of the nonce and the players choice is the same as before (we use it in our StateMachine definition below)
check :: BuiltinByteString -> BuiltinByteString -> GameDatum -> GameRedeemer -> ScriptContext -> Bool
check bsZero' bsOne' (GameDatum bs (Just c)) (Reveal nonce) _ =
    sha2_256 (nonce `appendByteString` if c == Zero then bsZero' else bsOne') == bs
check _       _      _                       _              _ = True  -- All other checks can return true (to not interfere with the transition checks above)

-- data State s = State {
--     stateData :: s           -- This is the state itself (in this case, the Datum)
--     stateValue :: Value      -- Because a StateMachine is represented by a UTxO, this UTxO has a datum and a Value
-- }

-- data ThreadToken = ThreadToken {
--     ttOutRef :: TxOutRef                 -- This UTxO is used to uniquely identify the minting txn of the NFT
--     ttCurrencySymbol :: CurrencySymbol
-- }

-- data StateMachine s i  = StateMachine {              -- Has two type parameters 's' (the state / datum) and 'i' (input / redeemer)
--         smTransition :: State s -> i -> Maybe (TxConstraints Void Void, State s)    ... defines from which state and using which transition you can get a new state
                                                                        -- we return nothing if the transition is not allowed
                                                                        -- if it's allowed we return additional constraints this trancition may have and the new State
--         smFinal :: s -> Bool                           ... defines whether this is a final state or not (a new state / UTxO isn't produced)
--         smCheck :: s -> i -> ScriptContext -> Bool     ... takes the datum, redeemer and context and checks for conditions that can't be expressed by TxConstraints
--         smThreadToken :: Maybe ThreadToken             ... used like the NFT in the EvenOdd example(the StateMachine will automatically mint the NFT and then thread
                                                                -- it through state transitions (implicitly include it in the UTxOs), and burn it on the final state)
-- } 

-- Here, we define the state machine
{-# INLINABLE gameStateMachine #-}
gameStateMachine :: Game -> BuiltinByteString -> BuiltinByteString -> StateMachine GameDatum GameRedeemer
gameStateMachine game bsZero' bsOne' = StateMachine
    { smTransition  = transition game
    , smFinal       = final
    , smCheck       = check bsZero' bsOne'  -- we check for additional conditions here (in this case, the nonce) ... this is True by default
    , smThreadToken = Just $ gToken game    -- we set the ThreadToken field in the StateMachine and it will handle the threading/passing of the NFT automatically
    }

{-# INLINABLE mkGameValidator #-}
                -- Game rules -> Bytestring versions of the choices -> Datum -> Redeemer -> Context
mkGameValidator :: Game -> BuiltinByteString -> BuiltinByteString -> GameDatum -> GameRedeemer -> ScriptContext -> Bool
mkGameValidator game bsZero' bsOne' = mkValidator $ gameStateMachine game bsZero' bsOne'

type Gaming = StateMachine GameDatum GameRedeemer   -- ???

bsZero, bsOne :: BuiltinByteString
bsZero = "0"
bsOne  = "1"

-- This is an alternative helper function to use on the validator above so that we only have to specify the game (no need to provide the two additional params
        -- Datum and Redeemer), we also use it below in the Off-chain code
gameStateMachine' :: Game -> StateMachine GameDatum GameRedeemer
gameStateMachine' game = gameStateMachine game bsZero bsOne

typedGameValidator :: Game -> Scripts.TypedValidator Gaming
typedGameValidator game = Scripts.mkTypedValidator @Gaming
    ($$(PlutusTx.compile [|| mkGameValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode game
        `PlutusTx.applyCode` PlutusTx.liftCode bsZero
        `PlutusTx.applyCode` PlutusTx.liftCode bsOne)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @GameDatum @GameRedeemer

gameValidator :: Game -> Validator
gameValidator = Scripts.validatorScript . typedGameValidator

gameAddress :: Game -> Ledger.Address
gameAddress = scriptAddress . gameValidator

-- =================== OFF CHAIN Code =========================

-- data StateMachineClient s i = StateMachineClient {
--     scInstance :: StateMachineInstance s i	
--     scChooser :: [OnChainState s i] -> Either SMContractError (OnChainState s i)	
                -- Had we not used the ThreadToken mechanism, we could have several UTxOs sitting at the address of the StateMachine, the Chooser is a way to 
                    -- select the right one. We give it a list of UTXxOs and it will return Either an error or the right UTxO. By default, if we used ThreadTokens,
                    -- the Chooser will choose the one UTxO with the token.
-- }

-- data StateMachineInstance s i = StateMachineInstance {
--     stateMachine :: StateMachine s i	
--     typedValidator :: TypedValidator (StateMachine s i)
-- }

gameClient :: Game -> StateMachineClient GameDatum GameRedeemer -- We use this to interact with our state machine from our Contract Monad
gameClient game = mkStateMachineClient $ StateMachineInstance (gameStateMachine' game) (typedGameValidator game) -- the default chooser will be used

data FirstParams = FirstParams
    { fpSecond         :: !PaymentPubKeyHash
    , fpStake          :: !Integer
    , fpPlayDeadline   :: !POSIXTime
    , fpRevealDeadline :: !POSIXTime
    , fpNonce          :: !BuiltinByteString
    , fpChoice         :: !GameChoice
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

-- Helper function that returns errors in the form of Text error messages
mapError' :: Contract w s SMContractError a -> Contract w s Text a
mapError' = mapError $ pack . show

waitUntilTimeHasPassed :: AsContractError e => POSIXTime -> Contract w s e ()
waitUntilTimeHasPassed t = void $ awaitTime t >> waitNSlots 1

firstGame :: forall s. FirstParams -> Contract (Last ThreadToken) s Text ()
firstGame fp = do
    pkh <- Contract.ownPaymentPubKeyHash
    tt  <- mapError' getThreadToken     -- Will use a UTxO (that'll be spent) from our wallet to use it for minting the ThreadToken
    let game   = Game
            { gFirst          = pkh
            , gSecond         = fpSecond fp             -- * These are taken from the parameter 'fp' given by the user (in the Emulator)
            , gStake          = fpStake fp              -- *
            , gPlayDeadline   = fpPlayDeadline fp       -- *
            , gRevealDeadline = fpRevealDeadline fp     -- * upto here
            , gToken          = tt
            }
        client = gameClient game
        v      = lovelaceValueOf (fpStake fp)
        c      = fpChoice fp
        bs     = sha2_256 $ fpNonce fp `appendByteString` if c == Zero then bsZero else bsOne
    void $ mapError' $ runInitialise client (GameDatum bs Nothing) v    -- runInitialise :: client -> Datum -> Value(stake)
        -- This will first mint the NFT corresponding to the ThreadToken
        -- Then it'll create a UTxO (with the NFT, Datum and Value(stake)) at the StateMachine address to start the StateMachine
    logInfo @String $ "made first move: " ++ show (fpChoice fp) -- Log the move we made
    tell $ Last $ Just tt   -- To 'tell' the ThreadToken to the second player ???

    waitUntilTimeHasPassed $ fpPlayDeadline fp

    m <- mapError' $ getOnChainState client
    case m of
        Nothing     -> throwError "game output not found"
        Just (o, _) -> case tyTxOutData $ ocsTxOut o of

            GameDatum _ Nothing -> do
                logInfo @String "second player did not play"
                void $ mapError' $ runStep client ClaimFirst
                logInfo @String "first player reclaimed stake"

            GameDatum _ (Just c') | c' == c -> do
                logInfo @String "second player played and lost"
                void $ mapError' $ runStep client $ Reveal $ fpNonce fp
                logInfo @String "first player revealed and won"

            _ -> logInfo @String "second player played and won"

data SecondParams = SecondParams
    { spFirst          :: !PaymentPubKeyHash
    , spStake          :: !Integer
    , spPlayDeadline   :: !POSIXTime
    , spRevealDeadline :: !POSIXTime
    , spChoice         :: !GameChoice
    , spToken          :: !ThreadToken
    } deriving (Show, Generic, FromJSON, ToJSON)

secondGame :: forall w s. SecondParams -> Contract w s Text ()
secondGame sp = do
    pkh <- Contract.ownPaymentPubKeyHash
    let game   = Game
            { gFirst          = spFirst sp
            , gSecond         = pkh
            , gStake          = spStake sp
            , gPlayDeadline   = spPlayDeadline sp
            , gRevealDeadline = spRevealDeadline sp
            , gToken          = spToken sp
            }
        client = gameClient game
    m <- mapError' $ getOnChainState client
    case m of
        Nothing          -> logInfo @String "no running game found"
        Just (o, _) -> case tyTxOutData $ ocsTxOut o of
            GameDatum _ Nothing -> do
                logInfo @String "running game found"
                void $ mapError' $ runStep client $ Play $ spChoice sp
                logInfo @String $ "made second move: " ++ show (spChoice sp)

                waitUntilTimeHasPassed $ spRevealDeadline sp

                m' <- mapError' $ getOnChainState client
                case m' of
                    Nothing -> logInfo @String "first player won"
                    Just _  -> do
                        logInfo @String "first player didn't reveal"
                        void $ mapError' $ runStep client ClaimSecond
                        logInfo @String "second player won"

            _ -> throwError "unexpected datum"

type GameSchema = Endpoint "first" FirstParams .\/ Endpoint "second" SecondParams

endpoints :: Contract (Last ThreadToken) GameSchema Text ()
endpoints = awaitPromise (first `select` second) >> endpoints
  where
    first  = endpoint @"first"  firstGame
    second = endpoint @"second" secondGame

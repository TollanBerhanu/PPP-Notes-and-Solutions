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

module Week07.EvenOdd
    ( Game (..)
    , GameChoice (..)
    , FirstParams (..)
    , SecondParams (..)
    , GameSchema
    , endpoints
    ) where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (FromJSON, ToJSON)
import qualified Data.Map             as Map
import           Data.Text            (Text)
import           GHC.Generics         (Generic)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Ledger.Value
import           Playground.Contract  (ToSchema)
import           Plutus.Contract      as Contract
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Prelude              (Semigroup (..), Show (..), String)
import qualified Prelude

-- ====================== ON CHAIN Code ===============================

-- This is given as a parameter to the contract
data Game = Game
    { gFirst          :: !PaymentPubKeyHash     -- Pubkey of the Player 1
    , gSecond         :: !PaymentPubKeyHash     -- Pubkey of the Player 2
    , gStake          :: !Integer               -- amount of lovelace used as stake by each player
    , gPlayDeadline   :: !POSIXTime             -- time the second player can make a move before the first player can claim back its stake
    , gRevealDeadline :: !POSIXTime             -- time the first player can claim victory by revealing its nonce, given the second player has made a move
    , gToken          :: !AssetClass            -- This is the NFT thats passed around in each turn (to validate whose turn it is and this allows only one valid txn
                                                    -- to be sent by each player on every turn).. because anyone can send UTxOs to the same address with the same datum
                                                    -- but only one can contain the NFT as pat of its Value
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)


-- The state is every move made by each player
    -- 1. State : Hash (nonce + Player 1 choice)
    -- 2. State : Hash (nonce + Player 1 choice) + Player 2 choice
-- The state gets updated on each step (Data is not immutable in ethereum), but in cardano, we spend the UTxO containing the state and create a new UTxO containing
-- the updated state... (but how do we know the new UTxO is an extension of the old one)
    -- To answer this, we use NFTs inside the UTxO
            -- Player 1 -> New State/UTxO : NFT + Hash (nonce + Player 1 choice) :: Datum
            -- Player 2 -> Consume old UTxO ->  New State/UTxO : NFT + Player 2 choice :: Datum

PlutusTx.makeLift ''Game

data GameChoice = Zero | One
    deriving (Show, Generic, FromJSON, ToJSON, ToSchema, Prelude.Eq, Prelude.Ord)

instance Eq GameChoice where
    {-# INLINABLE (==) #-}
    Zero == Zero = True
    One  == One  = True
    _    == _    = False

PlutusTx.unstableMakeIsData ''GameChoice
data GameDatum = GameDatum BuiltinByteString (Maybe GameChoice)     -- This is the state of the contract
    deriving Show
            -- BuiltinByteString: The hash submitted by the first player
            -- (Maybe GameChoice): The move by the second player (Nothing -> the 2nd player hasn't moved yet,  Just sth -> the 2nd player has moved)

instance Eq GameDatum where
    {-# INLINABLE (==) #-}
    GameDatum bs mc == GameDatum bs' mc' = (bs == bs') && (mc == mc')

PlutusTx.unstableMakeIsData ''GameDatum

data GameRedeemer = Play GameChoice | Reveal BuiltinByteString | ClaimFirst | ClaimSecond
    deriving Show
            -- Play: when the second player moves with a GameChoice (zero / one)
            -- Reveal: when the first player has won and must prove / reveal its nonce, hence the BuiltinByteString. There is no need to provide the move of the 
                        -- player because revealing implicitly means winning 
            -- ClaimFirst: the first player can claim back its stake (First player wins and takes back the NFT)
            -- ClaimSecond: if the first player doesn't reveal (because it lost), the second player can claim the winnings & the NFT

PlutusTx.unstableMakeIsData ''GameRedeemer

-- These are some helper functions
{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer   -- extract the amount of lovelaces contained in a Value type
lovelaces = Ada.getLovelace . Ada.fromValue

{-# INLINABLE gameDatum #-}
gameDatum :: Maybe Datum -> Maybe GameDatum -- Deseralize the Datum into our GameDatum
gameDatum md = do
    Datum d <- md
    PlutusTx.fromBuiltinData d

{-# INLINABLE mkGameValidator #-}
mkGameValidator :: Game -> BuiltinByteString -> BuiltinByteString -> GameDatum -> GameRedeemer -> ScriptContext -> Bool
mkGameValidator game bsZero' bsOne' dat red ctx =
        -- Check if the input contains the NFT
    traceIfFalse "token missing from input" (assetClassValueOf (txOutValue ownInput) (gToken game) == 1) &&
    -- (GameDatum Hash_1st_Player_Move 2nd_player_Move,  GameRedeemer Current_player_Move)
    case (dat, red) of
            -- First player has moved, but second player is moving ... this is the txn in w/c the second player moves
        (GameDatum bs Nothing, Play c) ->
            traceIfFalse "not signed by second player"   (txSignedBy info (unPaymentPubKeyHash $ gSecond game))             &&
            traceIfFalse "first player's stake missing"  (lovelaces (txOutValue ownInput) == gStake game)                   && -- check staked UTxO in player1's i/p
            traceIfFalse "second player's stake missing" (lovelaces (txOutValue ownOutput) == (2 * gStake game))            && -- check staked UTxO in player2's o/p
                                        -- we made this 2 * stake because the second player consumes the first player's UTxO and stakes the first players + his own
            traceIfFalse "wrong output datum"            (outputDatum == GameDatum bs (Just c))                             &&  -- Datum must contain tha same val as 
                                                                                                                            -- before with the 2nd player's move added
            traceIfFalse "missed deadline"               (to (gPlayDeadline game) `contains` txInfoValidRange info)         &&  -- 2nd player must submit before the play deadline
            traceIfFalse "token missing from output"     (assetClassValueOf (txOutValue ownOutput) (gToken game) == 1)          -- check NFT in player 2's output

        -- Both players have moved and the first player discovers that it has won ... (It has to reveal the nonce to prove that and claim winnings)
        (GameDatum bs (Just c), Reveal nonce) ->
            traceIfFalse "not signed by first player"    (txSignedBy info (unPaymentPubKeyHash $ gFirst game))              &&
            traceIfFalse "commit mismatch"               (checkNonce bs nonce c)                                            &&  -- The nonce must dive a valid hash
            traceIfFalse "missed deadline"               (to (gRevealDeadline game) `contains` txInfoValidRange info)       &&  -- 1st player must reveal before the reveal deadline
            traceIfFalse "wrong stake"                   (lovelaces (txOutValue ownInput) == (2 * gStake game))             &&  -- The input must contain the stake of both 
                                                                                                                        -- players, and the winner takes all
            traceIfFalse "NFT must go to first player"   nftToFirst                                                             -- The NFT must go back to the 1st player

        -- Second player hasn't moved and the deadline passes (1st player gets its stake back)
        (GameDatum _ Nothing, ClaimFirst) ->
            traceIfFalse "not signed by first player"    (txSignedBy info (unPaymentPubKeyHash $ gFirst game))              &&
            traceIfFalse "too early"                     (from (1 + gPlayDeadline game) `contains` txInfoValidRange info)   &&  -- 1st player can take back its stake only after the deadline has passed
            traceIfFalse "first player's stake missing"  (lovelaces (txOutValue ownInput) == gStake game)                   &&  -- 1st player gets its stake
            traceIfFalse "NFT must go to first player"   nftToFirst                                                                 -- and its NFT back

        -- Both players move but first player doesn't reveal its nonce (2nd player can claim its winnings)
        (GameDatum _ (Just _), ClaimSecond) ->
            traceIfFalse "not signed by second player"   (txSignedBy info (unPaymentPubKeyHash $ gSecond game))             &&
            traceIfFalse "too early"                     (from (1 + gRevealDeadline game) `contains` txInfoValidRange info) &&
            traceIfFalse "wrong stake"                   (lovelaces (txOutValue ownInput) == (2 * gStake game))             &&  -- Both players must have provided the stake
            traceIfFalse "NFT must go to first player"   nftToFirst                                                             -- NFT still goes back to the originator
                                                                                                            --  of the game (1st player) incase they want to play again

        _ -> False
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput :: TxOut
    ownInput = case findOwnInput ctx of -- findOwnInput returns a (Maybe TxInInfo) ... it can be Nothing if this script is used for minting
        Nothing -> traceError "game input missing"
        Just i  -> txInInfoResolved i   -- txInInfoResolved :: TxOut ... Actual output: (Address, Value, Maybe DatumHash)

    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of -- getContinuingOutputs returns [TxOut]... the output(s) that goes to the script address (normally, we expect one o/p)
        [o] -> o
        _   -> traceError "expected exactly one game output"

    -- the producer of the UTxO is required the actual datum to retrieve the datum
    outputDatum :: GameDatum    -- get the Datum at ownOutput
    outputDatum = case gameDatum $ txOutDatumHash ownOutput >>= flip findDatum info of  -- findDatum :: DatumHash -> TxInfo -> Maybe Datum (we apply the TxInfo to 
                                                                                                -- findDatum using flip (it flips the two arguments of a function))                           
        Nothing -> traceError "game output datum not found"
        Just d  -> d                                                                    -- this will return the datum if its included in the txn

    -- this is used to check if the choice given by the first player is legit after it won this round
               -- Hash it submitted -> the revealed nonce -> move both players made
    checkNonce :: BuiltinByteString -> BuiltinByteString -> GameChoice -> Bool
    checkNonce bs nonce cSecond = sha2_256 (nonce `appendByteString` cFirst) == bs
      where
        cFirst :: BuiltinByteString -- we need the BuiltinByteString version of the move made by the first player
        cFirst = case cSecond of    -- This function should run when the first player wins, so we are checking for what makes Player 1 win (in this case - same choice)
            Zero -> bsZero' 
            One  -> bsOne'

    -- Here we give the NFT back to the first player (the one who initiated the game), after the game is over (no matter who wins)
    nftToFirst :: Bool  -- Return true if the output of the txn implies that the NFT is paid to the first player
    nftToFirst = assetClassValueOf (valuePaidTo info $ unPaymentPubKeyHash $ gFirst game) (gToken game) == 1
    
    -- NOTE THAT: we are only checking the payment part here using valuePaidTo. Had this game included any staking rewards, we are not checking whether the staking
    -- rewards go to the first player or the winner ... this is a potential security risk

data Gaming
instance Scripts.ValidatorTypes Gaming where
    type instance DatumType Gaming = GameDatum
    type instance RedeemerType Gaming = GameRedeemer

bsZero, bsOne :: BuiltinByteString  -- Define the appropriate Strings to use as moves
bsZero = "0"
bsOne  = "1"

typedGameValidator :: Game -> Scripts.TypedValidator Gaming
typedGameValidator game = Scripts.mkTypedValidator @Gaming
    ($$(PlutusTx.compile [|| mkGameValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode game     -- Apply the game rules (The players, the staking amount, the deadlines, the NFT)
        `PlutusTx.applyCode` PlutusTx.liftCode bsZero
        `PlutusTx.applyCode` PlutusTx.liftCode bsOne)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @GameDatum @GameRedeemer

-- ====================== ON / OFF CHAIN =========================

gameValidator :: Game -> Validator
gameValidator = Scripts.validatorScript . typedGameValidator

gameAddress :: Game -> Ledger.Address
gameAddress = scriptAddress . gameValidator

-- ======================== OFF CHAIN Code =======================

-- This function will look for the NFT in the validator sctipt's address
findGameOutput :: Game -> Contract w s Text (Maybe (TxOutRef, ChainIndexTxOut, GameDatum))
findGameOutput game = do
    utxos <- utxosAt $ gameAddress game 
    return $ do
        (oref, o) <- find f $ Map.toList utxos
        dat       <- gameDatum $ either (const Nothing) Just $ _ciTxOutDatum o
        return (oref, o, dat)
  where
    f :: (TxOutRef, ChainIndexTxOut) -> Bool
    f (_, o) = assetClassValueOf (_ciTxOutValue o) (gToken game) == 1


waitUntilTimeHasPassed :: AsContractError e => POSIXTime -> Contract w s e ()
waitUntilTimeHasPassed t = do
    s1 <- currentSlot
    logInfo @String $ "current slot: " ++ show s1 ++ ", waiting until " ++ show t
    void $ awaitTime t >> waitNSlots 1
    s2 <- currentSlot
    logInfo @String $ "waited until: " ++ show s2

-- The inputs required by the first player to play the game
data FirstParams = FirstParams
    { fpSecond         :: !PaymentPubKeyHash    -- the opponent
    , fpStake          :: !Integer              
    , fpPlayDeadline   :: !POSIXTime
    , fpRevealDeadline :: !POSIXTime
    , fpNonce          :: !BuiltinByteString    -- the Nonce chosen by the first player
    , fpCurrency       :: !CurrencySymbol       -- the CurrencySymbol of the NFT
    , fpTokenName      :: !TokenName            -- the TokenName of the NFT
    , fpChoice         :: !GameChoice           -- the choice the first player makes
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

firstGame :: forall w s. FirstParams -> Contract w s Text ()
firstGame fp = do
    pkh <- Contract.ownPaymentPubKeyHash
    let game = Game     -- Define the game rules to apply to the game Validator
            { gFirst          = pkh
            , gSecond         = fpSecond fp
            , gStake          = fpStake fp
            , gPlayDeadline   = fpPlayDeadline fp
            , gRevealDeadline = fpRevealDeadline fp
            , gToken          = AssetClass (fpCurrency fp, fpTokenName fp)
            }
        v    = lovelaceValueOf (fpStake fp) <> assetClassValue (gToken game) 1      -- The stake and the NFT we put in to the output UTxO
        c    = fpChoice fp
        bs   = sha2_256 $ fpNonce fp `appendByteString` if c == Zero then bsZero else bsOne
        tx   = Constraints.mustPayToTheScript (GameDatum bs Nothing) v -- This is submitted by the first player (the second player hasn't played yet)
    ledgerTx <- submitTxConstraints (typedGameValidator game) tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ "made first move: " ++ show (fpChoice fp)

    waitUntilTimeHasPassed $ fpPlayDeadline fp

    m   <- findGameOutput game
    now <- currentTime
    case m of
        Nothing             -> throwError "game output not found"
        Just (oref, o, dat) -> case dat of
            GameDatum _ Nothing -> do               -- The second player hasn't moved before the deadline
                logInfo @String "second player did not play"
                let lookups = Constraints.unspentOutputs (Map.singleton oref o) <>
                              Constraints.otherScript (gameValidator game)
                    tx'     = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData ClaimFirst) <>
                              Constraints.mustValidateIn (from now)
                ledgerTx' <- submitTxConstraintsWith @Gaming lookups tx'
                void $ awaitTxConfirmed $ getCardanoTxId ledgerTx'
                logInfo @String "reclaimed stake"

            GameDatum _ (Just c') | c' == c -> do

                logInfo @String "second player played and lost"
                let lookups = Constraints.unspentOutputs (Map.singleton oref o) <>
                              Constraints.otherScript (gameValidator game)
                    tx'     = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ Reveal $ fpNonce fp) <>
                              Constraints.mustValidateIn (to $ now + 1000)
                ledgerTx' <- submitTxConstraintsWith @Gaming lookups tx'
                void $ awaitTxConfirmed $ getCardanoTxId ledgerTx'
                logInfo @String "victory"

            _ -> logInfo @String "second player played and won"

data SecondParams = SecondParams
    { spFirst          :: !PaymentPubKeyHash
    , spStake          :: !Integer
    , spPlayDeadline   :: !POSIXTime
    , spRevealDeadline :: !POSIXTime
    , spCurrency       :: !CurrencySymbol
    , spTokenName      :: !TokenName
    , spChoice         :: !GameChoice
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

secondGame :: forall w s. SecondParams -> Contract w s Text ()
secondGame sp = do
    pkh <- Contract.ownPaymentPubKeyHash
    let game = Game
            { gFirst          = spFirst sp
            , gSecond         = pkh
            , gStake          = spStake sp
            , gPlayDeadline   = spPlayDeadline sp
            , gRevealDeadline = spRevealDeadline sp
            , gToken          = AssetClass (spCurrency sp, spTokenName sp)
            }
    m <- findGameOutput game
    case m of
        Just (oref, o, GameDatum bs Nothing) -> do
            logInfo @String "running game found"
            now <- currentTime
            let token   = assetClassValue (gToken game) 1
            let v       = let x = lovelaceValueOf (spStake sp) in x <> x <> token
                c       = spChoice sp
                lookups = Constraints.unspentOutputs (Map.singleton oref o)                                   <>
                          Constraints.otherScript (gameValidator game)                                        <>
                          Constraints.typedValidatorLookups (typedGameValidator game)
                tx      = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ Play c) <>
                          Constraints.mustPayToTheScript (GameDatum bs $ Just c) v                            <>
                          Constraints.mustValidateIn (to now)
            ledgerTx <- submitTxConstraintsWith @Gaming lookups tx
            let tid = getCardanoTxId ledgerTx
            void $ awaitTxConfirmed tid
            logInfo @String $ "made second move: " ++ show (spChoice sp)

            waitUntilTimeHasPassed $ spRevealDeadline sp

            m'   <- findGameOutput game
            now' <- currentTime
            case m' of
                Nothing             -> logInfo @String "first player won"
                Just (oref', o', _) -> do
                    logInfo @String "first player didn't reveal"
                    let lookups' = Constraints.unspentOutputs (Map.singleton oref' o')                                     <>
                                   Constraints.otherScript (gameValidator game)
                        tx'      = Constraints.mustSpendScriptOutput oref' (Redeemer $ PlutusTx.toBuiltinData ClaimSecond) <>
                                   Constraints.mustValidateIn (from now')                                                  <>
                                   Constraints.mustPayToPubKey (spFirst sp) (token <> adaValueOf (getAda minAdaTxOut))
                    ledgerTx' <- submitTxConstraintsWith @Gaming lookups' tx'
                    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx'
                    logInfo @String "second player won"

        _ -> logInfo @String "no running game found"

type GameSchema = Endpoint "first" FirstParams .\/ Endpoint "second" SecondParams

endpoints :: Contract () GameSchema Text ()
endpoints = awaitPromise (first `select` second) >> endpoints
  where
    first  = endpoint @"first"  firstGame
    second = endpoint @"second" secondGame

module Main
    ( main
    ) where

import Control.Exception    (throwIO)
import Data.String          (IsString (..))
import System.Environment   (getArgs)
import Week06.Token.OnChain (tokenPolicy)
import Week06.Utils         (unsafeReadTxOutRef, writeMintingPolicy)

main :: IO ()
main = do
    [file, oref', amt', tn'] <- getArgs -- get arguments from the user: minting_policy_filename, ip_UTxO_for_NFT, amount_to_mint, TokenName
    let oref = unsafeReadTxOutRef oref'
        amt  = read amt'
        tn   = fromString tn'
        p    = tokenPolicy oref tn amt
    e <- writeMintingPolicy file p
    case e of
        Left err -> throwIO $ userError $ show err
        Right () -> return ()

-- we can run this in the terminal (nix-shell, but not in repl) as follows
    -- > cabal exec token-policy -- policy.plutus d3ae2834b29c534#0 100000 PPPToken
-- This was supposed to write the serialized mint-policy to file, had cabal worked properly
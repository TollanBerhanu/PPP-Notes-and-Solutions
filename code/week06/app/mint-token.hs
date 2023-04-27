{-# LANGUAGE OverloadedStrings  #-}

module Main
    ( main
    ) where

import Control.Exception          (throwIO)
import Data.String                (IsString (..))
import Network.HTTP.Req                 -- Http client library for haskell (we use it to request the PAB running on our machine)
import System.Environment         (getArgs)
import Text.Printf                (printf)
import Wallet.Emulator.Wallet     (WalletId (..))
import Wallet.Types               (ContractInstanceId (..))
import Week06.PAB                 (TokenContracts (..))
import Week06.Token.OffChain      (TokenParams (..))
import Week06.Utils               (contractActivationArgs, unsafeReadAddress, unsafeReadWalletId)

main :: IO ()
main = do
    [amt', tn', wid', addr'] <- getArgs -- take 4 params (amount, TokenName, wallet_id, address(owner of the token))
    let wid = unsafeReadWalletId wid'   -- convert the wid' string into an actual wallet_id
        tp  = TokenParams
                { tpToken   = fromString tn'
                , tpAmount  = read amt'
                , tpAddress = unsafeReadAddress addr'   -- convert the addr' string into an actual address
                }
    printf "minting token for wallet id %s with parameters %s\n" (show wid) $ show tp
    cid <- mintToken wid tp
    printf "minted tokens, contract instance id: %s\n" $ show cid


-- THis function is used to mint native tokens using only Haskell... we just do it through the PAB
mintToken :: WalletId -> TokenParams -> IO ContractInstanceId
mintToken wid tp = do
    v <- runReq defaultHttpConfig $ req
        POST                                                        -- request method
        (http "127.0.0.1" /: "api"  /: "contract" /: "activate")    -- url
        (ReqBodyJson $ contractActivationArgs wid $ Mint tp)        -- request body, contractActivationArgs is a helper function defined in Utils
        jsonResponse                                                -- we expect a json response
        (port 9080)
    let c = responseStatusCode v    -- our response is 'v'
    if c == 200             -- check if status code is 200
        then return $ responseBody v    -- our response body will be of type 'ContractInstanceId'
        else throwIO $ userError $ printf "ERROR: %d\n" c


-- this is run in the mint-token-haskell.sh script file
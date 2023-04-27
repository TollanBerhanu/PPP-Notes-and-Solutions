{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Week06.PAB
    ( Address
    , TokenContracts (..)
    ) where

import           Data.Aeson                          (FromJSON, ToJSON)
import           Data.OpenApi.Schema                 (ToSchema)
import           GHC.Generics                        (Generic)
import           Ledger                              (Address)
import           Plutus.PAB.Effects.Contract.Builtin (Empty, HasDefinitions (..), SomeBuiltin (..), endpointsToSchemas)
import           Prettyprinter                       (Pretty (..), viaShow)
import           Wallet.Emulator.Wallet              (knownWallet, mockWalletAddress)

import qualified Week06.Monitor                      as Monitor
import qualified Week06.Token.OffChain               as Token

data TokenContracts = Mint Token.TokenParams | Monitor Address
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON, ToSchema)

instance Pretty TokenContracts where
    pretty = viaShow

instance HasDefinitions TokenContracts where

    getDefinitions        = [Mint exampleTP, Monitor exampleAddr]

    -- getContract is used to define which Contract to run given a value of some type (in this case: Mint Token.TokenParams | Monitor Address)
        -- Notice that Token and Monitor are modules imported from this week's folder
    getContract (Mint tp)      = SomeBuiltin $ Token.mintToken @() @Empty tp
    getContract (Monitor addr) = SomeBuiltin $ Monitor.monitor addr

    getSchema = const $ endpointsToSchemas @Empty -- provide the schema for values of the above types
        -- we also could have done this, but we can write it in one line as above because both can be @Empty
    -- getSchema (Mint _) = endpointsToSchemas @Empty
    -- getSchema (Monitor _) = endpointsToSchemas @Empty

-- This is an example address we use above
exampleAddr :: Address
exampleAddr = mockWalletAddress $ knownWallet 1

-- This are example TokenParams we use above
exampleTP :: Token.TokenParams
exampleTP = Token.TokenParams
    { Token.tpAddress = exampleAddr
    , Token.tpAmount  = 123456
    , Token.tpToken   = "PPP"
    }

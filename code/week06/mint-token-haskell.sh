#!/bin/bash
# to mint a token using haskell (mint-token.hs) while the PAB is running

amt=$1
tn=$2
echo "minting $1 coins of token $2"

cabal run mint-token -- $1 $2 $WALLETID $ADDRESS
# takes the amount and token name, fills in wallet_id and address from environment variables
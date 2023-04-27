#!/bin/bash
# We need to run this before we start the PAB, if we are running the PAB for the first time (if there is no database created yet, this will
# create an empty database for us)
cabal run -- token-pab \
  --config testnet/pab-config.yml migrate

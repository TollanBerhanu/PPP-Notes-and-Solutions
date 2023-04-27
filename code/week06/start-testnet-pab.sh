#!/bin/bash
# to start the plutus application backend
cabal run -- token-pab \
  --config testnet/pab-config.yml webserver \
  --passphrase pab123456789
# the last attribute should be the pass phrase of our own wallet
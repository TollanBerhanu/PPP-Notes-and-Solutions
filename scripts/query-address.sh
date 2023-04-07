#!/bin/bash
cardano-cli query utxo --address "$1" --testnet-magic 2 # this is the preview test-net
# testnet-magic 1 is the pre-production test-net

# running ... scripts/query-address.sh $(cat keys/alice.addr) ... would fetch (TxHash, TxIx(Txn Index), Amount) for alice
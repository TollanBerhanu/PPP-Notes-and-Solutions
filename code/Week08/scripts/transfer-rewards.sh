#!/bin/bash

txin=$1
tmp=/workspace/code/Week08/tmp
body=$tmp/tx.txbody
signed=$tmp/tx.tx

user1=/workspace/cardano-private-testnet-setup/private-testnet/addresses/payment1.addr
user1_stake=/workspace/code/Week08/tmp/user1-script.addr

export CARDANO_NODE_SOCKET_PATH=/workspace/cardano-private-testnet-setup/private-testnet/node-spo1/node.sock

# Build the transaction
cardano-cli transaction build \
    --babbage-era \
    --testnet-magic 42 \
    --tx-in "$txin" \
    --tx-out "$(cat $user1) + 19999983124 lovelace" \
    --tx-out-inline-datum-file "/workspace/code/Week08/assets/unit.json" \
    --change-address "$(cat $user1_stake)" \
    --out-file "$body"
    
cardano-cli transaction sign \
    --testnet-magic 42 \
    --tx-body-file $body \
    --out-file $signed \
    --signing-key-file /workspace/cardano-private-testnet-setup/private-testnet/stake-delegator-keys/payment1.skey

cardano-cli transaction submit \
    --testnet-magic 42 \
    --tx-file $signed
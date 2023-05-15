#!/bin/bash
export CARDANO_NODE_SOCKET_PATH=/workspace/cardano-private-testnet-setup/private-testnet/node-spo1/node.sock

# Query the funds at the payment address of the script (the address is built using user 1's vkey)
cardano-cli query utxo \
    --testnet-magic 42 \
    --address $(cat /workspace/code/Week08/tmp/user1-script.addr)

#!/bin/bash
export CARDANO_NODE_SOCKET_PATH=/workspace/cardano-private-testnet-setup/private-testnet/node-spo1/node.sock

# Check the rewards accumulated for user 1 after delegating to a pool... the rewards will be sent to user 1's staking address
cardano-cli query stake-address-info \
    --testnet-magic 42 \
    --address $(cat /workspace/code/Week08/tmp/user1-script-stake.addr)

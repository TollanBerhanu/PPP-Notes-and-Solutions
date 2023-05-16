# This script (list of commands) takes all the funds from the gift address
#!/bin/bash

# Just define some vars
assets=/workspace/code/Week02/assets
keypath=/workspace/keys
name="$1" # specify the name of the address that will take the funds
collateral="$2" # specify some UTxO that you own incase your txn fails / the script fails, to pay for gas fees.
                # In practice, if sth is wrong, the node (miner) you are submitting your txn to will detect that and simply won't
                # submit your txn to the network and you don't have to pay anything. But if you circumvent that maliciously, then
                # you could lose the collateral (about 4 Ada)
txin="$3" # UTxO in the senders wallet (to pay for txn fees), but in this case we can pay from the gift we collect
txin2="$4" # UTxO in the senders wallet (to pay for txn fees), but in this case we can pay from the gift we collect

pp="$assets/protocol-parameters.json"
body="$assets/collect-gift.txbody"
tx="$assets/collect-gift.tx"

# Query the protocol parameters \

cardano-cli query protocol-parameters \
    --testnet-magic 2 \
    --out-file "$pp"

# Build the transaction
cardano-cli transaction build \
    --babbage-era \
    --testnet-magic 2 \
    --tx-in "$txin" \
    --tx-in-script-file "$assets/gift.plutus" \
    --tx-in-inline-datum-present \
    --tx-in-redeemer-file "$assets/unit.json" \
    --tx-in "$txin2" \
    --tx-in-inline-datum-present \
    --tx-in-redeemer-file "$assets/unit.json" \
    --tx-in-collateral "$collateral" \
    --change-address "$(cat "$keypath/$name.addr")" \
    --protocol-params-file "$pp" \
    --out-file "$body"
    # ~,~ (skipped lines ... they're are explained in make-gift.sh)
    # specify the input UTxOs of the txn (for txn fees)
    # specify the serialized script (after the Vasil hard fork, there exists an alternative where we don't have to include the script in the txn, using reference scripts)
        # the first txn doesn't need to do this (if you send sth to a script address, we don't need the actual script, because inorder
        # to validate the txn, the script need not be executed... validation occurs only when we spend from a script o/p, not when you 
        # create one. That means anybody can send anything anywhere(script, wallet, persons_addr) without validation). In this case,
        # we are trying to spend the UTxO located at the script address (this requires validation)
    # specify the datum (we just include an inline datum... that we don't use). We don't explicitly include the datum.
    # specify the redeemer (Just include some arbitrary data ... we don't use the redeemer too)
    # ~,~
    # the protocol parameter specified because this txn involves smart contract execution and the costs for executing it plus the txn
        # fees needs to be calculated. The protocol parameter file should specify the cost of certain operations of a smart contract.
        # This was not required in the first txn (make-gift) because it doesn't execute a smart contract for validation (we were just 
        # sending sth to a smart contract)
    # specify where to write the txn

# The rest is the same as make-gift.sh

# Sign the transaction
cardano-cli transaction sign \
    --tx-body-file "$body" \
    --signing-key-file "$keypath/$name.skey" \
    --testnet-magic 2 \
    --out-file "$tx"

# Submit the transaction
cardano-cli transaction submit \
    --testnet-magic 2 \
    --tx-file "$tx"

tid=$(cardano-cli transaction txid --tx-file "$tx")
echo "transaction id: $tid"
echo "Cardanoscan: https://preview.cardanoscan.io/transaction/$tid"
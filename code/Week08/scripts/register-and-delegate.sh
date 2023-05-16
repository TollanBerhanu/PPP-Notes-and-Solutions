#!/bin/bash

tmp=/workspace/code/Week08/tmp
txin=$1     # Input UTxO of user 1 to delegate to pool 1
echo "txin: $txin"

script=/workspace/code/Week08/assets/staking.plutus     # The serialized script
script_stake_addr=$tmp/user1-script-stake.addr          # File to write the stake address of user 1 for the script  (place to put staking rewards) ?
script_payment_addr=$tmp/user1-script.addr              # File to write the payment address of user 1 for the script
registration=$tmp/registration.cert                     # File to write the registration certificate
delegation=$tmp/delegation.cert                         # File to write the delegation certificate
pp=$tmp/protocol-params.json                            # File to write the protocol parameters
body=$tmp/tx.txbody                                     # File to write the built transaction
signed=$tmp/tx.tx                                       # File to write the signed transaction

normaddr=$tmp/norm.addr

# Set the node.socket path
export CARDANO_NODE_SOCKET_PATH=/workspace/cardano-private-testnet-setup/private-testnet/node-spo1/node.sock

# Build the stake address from the serialized script
cardano-cli stake-address build \
    --testnet-magic 42 \
    --stake-script-file $script \
    --out-file $script_stake_addr

echo "stake address: $(cat $script_stake_addr)"

# Build the payment address from the serialized script (set the payment-verification-key to user 1's payment vkey and the staking part will be the new stake address)
cardano-cli address build \
    --testnet-magic 42 \
    --payment-verification-key-file=/workspace/cardano-private-testnet-setup/private-testnet/stake-delegator-keys/payment1.vkey \
    --stake-script-file=$script \
    --out-file $script_payment_addr

echo "payment address: $(cat $script_payment_addr)"

# Generate a registration certificate for the serialized script
cardano-cli stake-address registration-certificate \
    --stake-script-file $script \
    --out-file $registration

# Generate a delegation certificate for the serialized script and specify the pool to delegate to (we use first one we find by quering)
cardano-cli stake-address delegation-certificate \
    --stake-script-file $script \
    --stake-pool-id=$(/workspace/code/Week08/scripts/query-stake-pools.sh | head -n 1) \
    --out-file $delegation

# Get the protocol parameters from somewhere
cardano-cli query protocol-parameters \
    --testnet-magic 42 \
    --out-file $pp

# Build the transaction (we input user 1's UTxO and use the script_payment_addr as change address, we don't secify an output)
# We give it the serialized delegation and registration certificates which the node will try to execute (the script always returns true for any 'Certifying DCert' 
    # values of the ScriptPurpose)
# We have to provide the serialized script as witness (in this case, we are not using pub/pri staking keys)
cardano-cli transaction build \
    --babbage-era \
    --testnet-magic 42 \
    --change-address $(cat $script_payment_addr) \
    --out-file $body \
    --tx-in $txin \
    --tx-in-collateral $txin \
    --certificate-file $registration \
    --certificate-file $delegation \
    --certificate-script-file $script \
    --certificate-redeemer-file /workspace/code/Week08/assets/unit.json \
    --protocol-params-file $pp

# We sign the txn using user 1's payment skey because we spend their UTxO
cardano-cli transaction sign \
    --testnet-magic 42 \
    --tx-body-file $body \
    --out-file $signed \
    --signing-key-file /workspace/cardano-private-testnet-setup/private-testnet/stake-delegator-keys/payment1.skey

# Submit the signed txn
cardano-cli transaction submit \
    --testnet-magic 42 \
    --tx-file $signed

# After all this, we have registered the staking address for User 1 using a StakingValidator script and sent some funds to it.  ???
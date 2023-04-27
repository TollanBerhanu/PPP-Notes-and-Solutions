#!/bin/bash

#This shell script takes 5 parameters, we get the minting policy by running app/token-policy.hs using cabal, which is done below
oref=$1         # the UTxO we used to get the minting policy
amt=$2          # the amount of ada to be minted (also included in the minting policy)
tn=$3           # the token name (also included in the minting policy)
addrFile=$4     # the address of the signer, also the change address
skeyFile=$5     # the signing key

echo "oref: $oref"
echo "amt: $amt"
echo "tn: $tn"
echo "address file: $addrFile"
echo "signing key file: $skeyFile"

ppFile=testnet/protocol-parameters.json # these are protocol parameters
cardano-cli query protocol-parameters $MAGIC --out-file $ppFile # this is how we create protocol parameters using cardano cli and write them

policyFile=testnet/token.plutus
cabal exec token-policy $policyFile $oref $amt $tn # get the minting policy by applying the parameters we received above

unsignedFile=testnet/tx.unsigned
signedFile=testnet/tx.signed
pid=$(cardano-cli transaction policyid --script-file $policyFile) # compute the policy id / CurrencySymbol / hash of the script
tnHex=$(cabal exec token-name -- $tn)   # convert TokenName to Hex
addr=$(cat $addrFile)
v="$amt $pid.$tnHex" # This is the value we want to mint ... 10000 mypolicyid.tokenhex

echo "currency symbol: $pid"
echo "token name (hex): $tnHex"
echo "minted value: $v"
echo "address: $addr"

# This is where we build the txn to mint the NFT / token
cardano-cli transaction build \ 
    $MAGIC \
    --tx-in $oref \
    --tx-in-collateral $oref \
    --tx-out "$addr + 1500000 lovelace + $v" \
    --mint "$v" \
    --mint-script-file $policyFile \
    --mint-redeemer-file testnet/unit.json \
    --change-address $addr \
    --protocol-params-file $ppFile \
    --out-file $unsignedFile \
    # Notice how we specify the 'tx-out' in the command above ... "our_address + min_ada + value_we_mint"
    # The 'mint' argument specifies how much the txn should mint
        # mint expects the actual minting policy and a redeemer file

cardano-cli transaction sign \
    --tx-body-file $unsignedFile \
    --signing-key-file $skeyFile \
    $MAGIC \
    --out-file $signedFile

cardano-cli transaction submit \
    $MAGIC \
    --tx-file $signedFile

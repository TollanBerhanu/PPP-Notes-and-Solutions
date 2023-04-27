#!/bin/bash
# to tell the wallet backend about the newly created wallet ... you start the wallet backend by running the script 'start-testnet-wallet.sh'
# you send it the restore-wallet.json file via a POST request ... restore-wallet.json contains the seed phrases to your wallet
curl -H "content-type: application/json" -XPOST \
    -d @testnet/restore-wallet.json \
    localhost:8090/v2/wallets

# running this script will return a wallet_id (along with other stuff), which we can use to talk to the PAB... we set it as an environment variable in
# in the script 'env.sh'
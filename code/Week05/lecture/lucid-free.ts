import {
    Lucid,
    Blockfrost,
    Address,
    MintingPolicy,
    PolicyId,
    Unit,
    fromText,
    Data
} from "https://deno.land/x/lucid@0.9.1/mod.ts"
import { blockfrostKey, secretSeed } from "./secret.ts"

function readAmount(): bigint {
    const input = prompt("amount: "); // asks user to input amount
    return input ? BigInt(Number.parseInt(input)) : 1000000n; // Return 100000 if the user failed to input amount
}

const freePolicy: MintingPolicy = { // Our Free minting policy
    type: "PlutusV2",
    script: "5830582e010000323222320053333573466e1cd55ce9baa0024800080148c98c8014cd5ce249035054310000500349848005"
};

// set blockfrost endpoint
const lucid = await Lucid.new(
    new Blockfrost(
        "https://cardano-preview.blockfrost.io/api/v0",
        blockfrostKey
    ),
    "Preview"
);

// load local stored seed as a wallet into lucid
lucid.selectWalletFromSeed(secretSeed);
const addr: Address = await lucid.wallet.address();
console.log("own address: " + addr); // log out my public address

const policyId: PolicyId = lucid.utils.mintingPolicyToId(freePolicy);
console.log("minting policy: " + policyId); // log out the PolicyID/hash of serialized minting policy (CurrencySymbol)

const unit: Unit = policyId + fromText("PPP Free");
    // Unit is the representation of (AssetClass :: (CurrencySymbol, TokenName)) in lucid... it's just the concatenation of the
        // CurrencySymbol & the Hex representation of the token name (because TokenNames are arbitrary bytestrings (it's easier to 
        // work with them in Hex form), also cardano-cli expects them in Hex)

const amount: bigint = readAmount(); // read amount to mint from user

const tx = await lucid // Construct the txn
    .newTx()
    .mintAssets({[unit]: amount}, Data.void()) // mintAssets(assets: Assets, redeemer?: Redeemer): Tx
        // an object where the keys are the asset classes we want to mint and the value is the amount
        // all assets must have the same CurrencySymbol / PolicyId
        // we chain mintAssets functions if we want to mint assets with different policy ids.
    .attachMintingPolicy(freePolicy) // attachMintingPolicy(mintingPolicy: MintingPolicy): Tx
        // we attach the actual script / minting policy
        // we can also use reference scripts like validation scripts but minting policies are normally used only once 
            // (we normally mint native tokens once)
    .complete();
    // Lucid automatically includes UTxO from our wallet to pay for txn fees

const signedTx = await tx.sign().complete();
const txHash = await signedTx.submit();
console.log("tid: " + txHash);
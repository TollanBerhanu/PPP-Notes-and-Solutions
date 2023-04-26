import {
    Lucid,
    Blockfrost,
    Address,
    MintingPolicy,
    PolicyId,
    Unit,
    fromText,
    Data,
    getAddressDetails,
    applyParamsToScript
} from "https://deno.land/x/lucid@0.9.1/mod.ts"
import { blockfrostKey, secretSeed } from "../lecture/secret.ts"

function readAmount(): bigint {
    const input = prompt("amount: ");
    return input ? BigInt(Number.parseInt(input)) : 1000000n;
}

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
console.log("own address: " + addr);

const pkh: string = getAddressDetails(addr).paymentCredential?.hash || "";
console.log("own pubkey hash: " + pkh); // get my own PubKeyHash (hash of my public/payment address)

// Set the deadline to mint/burn native tokens
const deadlineDate: Date = new Date("2023-04-25T12:31:00Z")
const deadlinePosIx = BigInt(deadlineDate.getTime());   // Convert the deadline to POSIX time


const Params = Data.Tuple([Data.String, Data.BigInt]); // Tell lucid what type the parameter will have. Data.Tuple is used because we can have >1 param
type Params = Data.Static<typeof Params>;
const signedPolicy: MintingPolicy = {
    type: "PlutusV2",
    script: applyParamsToScript<Params>(
        "590a8e590a8b010000323232332232323232323232323232332232332232323232323332223232322222323253353232323232533553353235001222222222222533533355301912001321233001225335002210031001002502725335333573466e3c04c0040d40d04d40a4004540a0010840d440cd4004409c4cd5ce2481134f776e6572206861736e2774207369676e6564000261533535355001222222222222005223232350012235005232323232253335533350082153335007215333500a2130054984c011261533350082130054984c01126101f101d1533350092130054984c011261533350072130054984c01126101e1533350062101c101d101b15333500621533350092130064984c015261533350072130064984c01526101e101c1533350082130064984c015261533350062130064984c01526101d1533500715335001103210331032103310322533350052153335008215333500721333501c00c002001161616101c153335007215333500621333501b00b002001161616101b101a2533350042153335007215333500621333501b00b002001161616101b153335006215333500521333501a00a002001161616101a10192533350032153335006215333500521333501a00a002001161616101a1533350052153335004213335019009002001161616101910182533350022153335005215333500421333501900900200116161610191533350042153335003213335018008002001161616101810173350103502600702a123333333300122333573466e1c0080040b00ac894cd4ccd5cd19b8700200102c02b101615335333573466e240080040b00ac4050405488ccd5cd19b8800200102c02b22333573466e240080040b00ac88ccd5cd19b8900200102b02c22333573466e200080040ac0b0894cd4ccd5cd19b8900200102c02b10011002225335333573466e240080040b00ac40084004409c4cd5ce248113446561646c696e652068617320706173736564000261026135001220023333573466e1cd55cea802a4000466442466002006004646464646464646464646464646666ae68cdc39aab9d500c480008cccccccccccc88888888888848cccccccccccc00403403002c02802402001c01801401000c008cd407c080d5d0a80619a80f8101aba1500b33501f02135742a014666aa046eb94088d5d0a804999aa811bae502235742a01066a03e0586ae85401cccd5408c0b5d69aba150063232323333573466e1cd55cea801240004664424660020060046464646666ae68cdc39aab9d5002480008cc8848cc00400c008cd40ddd69aba150023039357426ae8940088c98c80eccd5ce01f01e81c89aab9e5001137540026ae854008c8c8c8cccd5cd19b8735573aa0049000119a81119a81bbad35742a00460726ae84d5d1280111931901d99ab9c03e03d039135573ca00226ea8004d5d09aba2500223263203733573807407206a26aae7940044dd50009aba1500533501f75c6ae854010ccd5408c0a48004d5d0a801999aa811bae200135742a00460566ae84d5d1280111931901999ab9c036035031135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d55cf280089baa00135742a00a60366ae84d5d1280291931901299ab9c0280270233333573466e1cd55ce9baa0064800080988c98c8090cd5ce0138130111bad006375c00c2046264c6404266ae712410350543500023135573ca00226ea8004c8004d5407888448894cd40044d400c88004884ccd401488008c010008ccd54c01c4800401401000448848cc00400c00848d40048888888801c488800c4888008488800448c88c008dd6000990009aa80c911999aab9f0012500a233500930043574200460066ae880080648c8c8cccd5cd19b8735573aa004900011991091980080180118071aba150023005357426ae8940088c98c805ccd5ce00d00c80a89aab9e5001137540024646464646666ae68cdc39aab9d5004480008cccc888848cccc00401401000c008c8c8c8cccd5cd19b8735573aa0049000119910919800801801180b9aba1500233500f016357426ae8940088c98c8070cd5ce00f80f00d09aab9e5001137540026ae854010ccd54021d728039aba150033232323333573466e1d4005200423212223002004357426aae79400c8cccd5cd19b875002480088c84888c004010dd71aba135573ca00846666ae68cdc3a801a400042444006464c6403c66ae7008408007006c0684d55cea80089baa00135742a00466a016eb8d5d09aba2500223263201833573803603402c26ae8940044d5d1280089aab9e500113754002266aa002eb9d6889119118011bab00132001355016223233335573e0044a010466a00e66442466002006004600c6aae754008c014d55cf280118021aba200301713574200222440042442446600200800624464646666ae68cdc3a800a400046a00e600a6ae84d55cf280191999ab9a3370ea00490011280391931900999ab9c016015011010135573aa00226ea800448488c00800c44880048c8c8cccd5cd19b875001480188c848888c010014c01cd5d09aab9e500323333573466e1d400920042321222230020053009357426aae7940108cccd5cd19b875003480088c848888c004014c01cd5d09aab9e500523333573466e1d40112000232122223003005375c6ae84d55cf280311931900899ab9c01401300f00e00d00c135573aa00226ea80048c8c8cccd5cd19b8735573aa004900011991091980080180118029aba15002375a6ae84d5d1280111931900699ab9c01000f00b135573ca00226ea80048c8cccd5cd19b8735573aa002900011bae357426aae7940088c98c802ccd5ce00700680489baa001232323232323333573466e1d4005200c21222222200323333573466e1d4009200a21222222200423333573466e1d400d2008233221222222233001009008375c6ae854014dd69aba135744a00a46666ae68cdc3a8022400c4664424444444660040120106eb8d5d0a8039bae357426ae89401c8cccd5cd19b875005480108cc8848888888cc018024020c030d5d0a8049bae357426ae8940248cccd5cd19b875006480088c848888888c01c020c034d5d09aab9e500b23333573466e1d401d2000232122222223005008300e357426aae7940308c98c8050cd5ce00b80b00900880800780700680609aab9d5004135573ca00626aae7940084d55cf280089baa0012323232323333573466e1d400520022333222122333001005004003375a6ae854010dd69aba15003375a6ae84d5d1280191999ab9a3370ea0049000119091180100198041aba135573ca00c464c6401a66ae7004003c02c0284d55cea80189aba25001135573ca00226ea80048c8c8cccd5cd19b875001480088c8488c00400cdd71aba135573ca00646666ae68cdc3a8012400046424460040066eb8d5d09aab9e500423263200a33573801a01801000e26aae7540044dd500089119191999ab9a3370ea00290021091100091999ab9a3370ea004900111a80398031aba135573ca00846666ae68cdc3a801a400042444004464c6401666ae7003803402402001c4d55cea80089baa00112122230030042323333573466e1d40052002200623333573466e1d40092000200623263200633573801201000800626aae74dd5000a4c2440042440022400292010350543100112323001001223300330020020011",
        [pkh, deadlinePosIx], // Here, we apply our PKH to the serialized minting script to make it a complete minting policy
        Params)
};
// This only works for parameters of type BuiltinData (in the on-chain code)

const policyId: PolicyId = lucid.utils.mintingPolicyToId(signedPolicy); // get the CurrencySymbol
console.log("minting policy: " + policyId);

const unit: Unit = policyId + fromText("PPP Deadline"); // represent the AssetClass (CurrencySymbol, TokenName)

const amount: bigint = readAmount(); // read the amount to be minted/burned from the user

const tx = await lucid // construct the txn
    .newTx()
    .mintAssets({[unit]: amount}, Data.void())
    .attachMintingPolicy(signedPolicy)
    .addSignerKey(pkh) // we need to explicitly state the pubKeyHash of the signer
    .validFrom(Date.now()-100000)
    .validTo(Date.now()+100000)
    .complete();

const signedTx = await tx.sign().complete(); // We sign it using out privKey and submit it
const txHash = await signedTx.submit();
console.log("tid: " + txHash);
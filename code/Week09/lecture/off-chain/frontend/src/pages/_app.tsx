import "@/styles/globals.css";
import {
    Address,
    Blockfrost,
    Lucid,
    MintingPolicy,
    PolicyId,
    SpendingValidator,
    TxHash,
    UTxO,
    Unit,
} from "lucid-cardano";
import type { AppProps } from "next/app";
import {
    Dispatch,
    SetStateAction,
    createContext,
    useEffect,
    useState,
} from "react";

const collateralScript: SpendingValidator = {
    type: "PlutusV2",
    script: "590ecf590ecc010000323233223322323232332232323232323232323232323232323233223232323232323223232323223223232533532323232323253335005153355001102a1335738921216275726e656420737461626c65636f696e20616d6f756e74206d69736d6174636800029153355335330115003350062222003102a1335738920124636f6c6c61746572616c206f776e65722773207369676e6174757265206d697373696e6700029153355335333573466e1cd40188888009200002a029102a133573892123696e697469616c20737461626c65636f696e20616d6f756e74206d75737420626520300002915335533553353500622220011029102a102a13357389211b636f6c6c61746572616c206d75737420626520756e6c6f636b656400029153355335333573466e254009200002902a102a13357389211e6d696e74656420616d6f756e74206d75737420626520706f736974697665000291533553355333535533553353500422350022222222222223333500d2503f2503f2503f23335530221200150212350012253355335333573466e3cd400888008d4010880080f00ec4ccd5cd19b873500222001350042200103c03b103b1350430031504200d2135001223500122223500b2235002222222222222333553029120012235002222253353501822350062232335005233500425335333573466e3c00800413012c5400c412c812c8cd4010812c94cd4ccd5cd19b8f00200104c04b15003104b153350032153350022133500223350022335002233500223303a002001204e2335002204e23303a00200122204e222335004204e2225335333573466e1c01800c14414054cd4ccd5cd19b870050020510501333573466e1c01000414414041404140412454cd40048412441244cd4124018014401541100284c98c80b8cd5ce249024c66000321302c4988854cd40044008884c0c126222200215030213017001232153353235001222222222222300e002500521301900115032320013550342253350011503322135002225335333573466e3c00801c0c40c04d40e00044c01800c854cd4ccd5cd19b8f35001222200435007222200402b02a15335333573466e3cd4004888800cd401c888800c0ac0a854cd4ccd5cd19b87350012222002500302b02a15335350012222001102b102a102a102a102a1029102a13357389211a696e76616c6964206e6577206f7574707574277320646174756d000291029102910291029153355335330115003350062222003102a1335738920124636f6c6c61746572616c206f776e65722773207369676e6174757265206d697373696e670002915335350062222001153355001102a13357389201216275726e656420737461626c65636f696e20616d6f756e74206d69736d6174636800029102a10291333573466e1ccdc0a40006a00a4444004a00205205026464646464600200a640026aa06a4466a0029000111a80111299a999ab9a3371e0040120640622600e0022600c006640026aa0684466a0029000111a80111299a999ab9a3371e00400e06206020022600c006640026e612401045553445000350052222004355001222222222222008135001220023333573466e1cd55cea80224000466442466002006004646464646464646464646464646666ae68cdc39aab9d500c480008cccccccccccc88888888888848cccccccccccc00403403002c02802402001c01801401000c008cd409809cd5d0a80619a8130139aba1500b33502602835742a014666aa054eb940a4d5d0a804999aa8153ae502935742a01066a04c05e6ae85401cccd540a80c1d69aba150063232323333573466e1cd55cea801240004664424660020060046464646666ae68cdc39aab9d5002480008cc8848cc00400c008cd40e9d69aba15002303b357426ae8940088c98c8104cd5ce01f02281f89aab9e5001137540026ae854008c8c8c8cccd5cd19b8735573aa004900011991091980080180119a81d3ad35742a00460766ae84d5d1280111931902099ab9c03e04503f135573ca00226ea8004d5d09aba2500223263203d33573807408207626aae7940044dd50009aba1500533502675c6ae854010ccd540a80b08004d5d0a801999aa8153ae200135742a004605c6ae84d5d1280111931901c99ab9c03603d037135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d55cf280089baa00135742a008603c6ae84d5d1280211931901599ab9c02802f0293333573466e1d401520042122200323333573466e1d401920022122200123333573466e1d401d20002122200223263202c3357380520600540520506666ae68cdc39aab9d5009480008cccc048dd71aba15009375c6ae854020dd69aba1500732323333573466e1d40052002201723333573466e1d40092000201723263202d33573805406205605426aae74dd50009aba135744a00e464c6405266ae700980b409c40b04c98c80a0cd5ce2481035054350002c135573ca00226ea80044d55ce9baa001135744a00226ae8940044d55cf280089baa0012235002222222222222533533355301012001500f25335333573466e3c0380040a009c4d40bc004540b8010840a040984c848cc004894cd40088400c40040094078c8004d5408088448894cd40044d400c88004884ccd401488008c010008ccd54c01c48004014010004c8004d5407c88448894cd40044008884cc014008ccd54c01c4800401401000488ccd5cd19b8f002001017016232323232323333333574800c46666ae68cdc39aab9d5006480008cccd55cfa8031281191999aab9f500625024233335573ea00c4a04a46666aae7d4018940988cccd55cf9aba2500725335533553355335301135742a016426a052601e0022a04e42a66a60246ae85402c84d40a8c008004540a05409c854cd4c8ccccccd5d200092815128151281511a8159bad0022502a02b35742a014426a05460040022a0502a04e42a66a646464646666666ae900108cccd5cd19b875002480088cccd55cfa80211a81800c1281781811999ab9a3370ea006900011999aab9f5005235031018250300312502f02b02a2502d2502d2502d2502d02e135573aa00426aae7940044dd50009aba15009213502a30020011502815027250270280270260250242502201e25021250212502125021022135744a00226ae8940044d5d1280089aab9e500113754002444424666600200a00800600424400424400246666666ae90004940609406094060940608d4064dd700100c8919118011bac001320013550192233335573e0024a030466a02e60086ae84008c00cd5d100100d119191999ab9a3370e6aae7540092000233221233001003002300a35742a004600a6ae84d5d1280111931900b19ab9c01301a014135573ca00226ea80048c8c8c8c8cccd5cd19b8735573aa00890001199991110919998008028020018011919191999ab9a3370e6aae7540092000233221233001003002301335742a00466a01a0246ae84d5d1280111931900d99ab9c01801f019135573ca00226ea8004d5d0a802199aa8043ae500735742a0066464646666ae68cdc3a800a4008464244460040086ae84d55cf280191999ab9a3370ea0049001119091118008021bae357426aae7940108cccd5cd19b875003480008488800c8c98c8074cd5ce00d01080d80d00c89aab9d5001137540026ae854008cd4025d71aba135744a004464c6402e66ae7005006c0544d5d1280089aba25001135573ca00226ea80044cd54005d73ad112232230023756002640026aa02c44646666aae7c008940588cd4054cc8848cc00400c008c018d55cea80118029aab9e500230043574400603026ae84004488c8c8cccd5cd19b875001480008d4058c014d5d09aab9e500323333573466e1d400920022501623263201433573802203002402226aae7540044dd5000919191999ab9a3370ea002900311909111180200298039aba135573ca00646666ae68cdc3a8012400846424444600400a60126ae84d55cf280211999ab9a3370ea006900111909111180080298039aba135573ca00a46666ae68cdc3a8022400046424444600600a6eb8d5d09aab9e500623263201433573802203002402202001e26aae7540044dd5000919191999ab9a3370e6aae7540092000233221233001003002300535742a0046eb4d5d09aba2500223263201033573801a02801c26aae7940044dd50009191999ab9a3370e6aae75400520002375c6ae84d55cf280111931900719ab9c00b01200c13754002464646464646666ae68cdc3a800a401842444444400646666ae68cdc3a8012401442444444400846666ae68cdc3a801a40104664424444444660020120106eb8d5d0a8029bad357426ae8940148cccd5cd19b875004480188cc8848888888cc008024020dd71aba15007375c6ae84d5d1280391999ab9a3370ea00a900211991091111111980300480418061aba15009375c6ae84d5d1280491999ab9a3370ea00c900111909111111180380418069aba135573ca01646666ae68cdc3a803a400046424444444600a010601c6ae84d55cf280611931900b99ab9c01401b01501401301201101000f135573aa00826aae79400c4d55cf280109aab9e5001137540024646464646666ae68cdc3a800a4004466644424466600200a0080066eb4d5d0a8021bad35742a0066eb4d5d09aba2500323333573466e1d4009200023212230020033008357426aae7940188c98c8040cd5ce00680a00700689aab9d5003135744a00226aae7940044dd5000919191999ab9a3370ea002900111909118008019bae357426aae79400c8cccd5cd19b875002480008c8488c00800cdd71aba135573ca008464c6401a66ae7002804402c0284d55cea80089baa00112232323333573466e1d400520042122200123333573466e1d40092002232122230030043006357426aae7940108cccd5cd19b87500348000848880088c98c8038cd5ce00580900600580509aab9d5001137540024646666ae68cdc3a800a4004400a46666ae68cdc3a80124000400a464c6401466ae7001c03802001c4d55ce9baa00112200212200149103505431002326320033357389212665787065637465642065786163746c79206f6e6520636f6c6c61746572616c206f7574707574000074984488008488488cc00401000c48488c00800c448800448004448c8c00400488cc00cc0080080041",
};

const collateralAddr: Address =
    "addr_test1wpwra8ssewfzlftpfx50wt4vfl4989gzuhy6werfztlmrug4qshwn";

export type AppState = {
    lucid?: Lucid;
    wAddr?: Address;
    nftPolicyIdHex?: PolicyId;
    nftTokenNameHex?: string;
    nftAssetClassHex?: Unit;
    nftPolicy?: MintingPolicy;
    scPolicyIdHex?: PolicyId;
    scTokenNameHex?: string;
    scAssetClassHex?: Unit;
    scPolicy?: MintingPolicy;
    oracleScript?: SpendingValidator;
    oracleAddress?: Address;
    oracleUTxOWithNFT?: UTxO;
    minPercent?: number;
    collateralScript: SpendingValidator;
    collatealAddr: Address;
    collateralRefScrUTxO?: UTxO;
    mpRefScrUTxO?: UTxO;
    txScripsDeployment?: TxHash;
};

const initialAppState: AppState = {
    collateralScript: collateralScript,
    collatealAddr: collateralAddr,
};

export const AppStateContext = createContext<{
    appState: AppState;
    setAppState: Dispatch<SetStateAction<AppState>>;
}>({ appState: initialAppState, setAppState: () => {} });

export default function App({ Component, pageProps }: AppProps) {
    const [appState, setAppState] = useState<AppState>(initialAppState);

    const connectLucidAndNami = async () => {
        const lucid = await Lucid.new(
            new Blockfrost(
                "https://cardano-preview.blockfrost.io/api/v0",
                "previewfz0NMrCf2gTuGYmnkzB4KfNmM3qzYBzL"
            ),
            "Preview"
        );
        if (!window.cardano.nami) {
            window.alert("Please install Nami Wallet");
            return;
        }
        const nami = await window.cardano.nami.enable();
        lucid.selectWallet(nami);
        setAppState({
            ...initialAppState,
            lucid: lucid,
            wAddr: await lucid.wallet.address(),
        });
    };

    useEffect(() => {
        if (appState.lucid) return;
        connectLucidAndNami();
    }, [appState]);
    return (
        <AppStateContext.Provider value={{ appState, setAppState }}>
            <Component {...pageProps} />
        </AppStateContext.Provider>
    );
}

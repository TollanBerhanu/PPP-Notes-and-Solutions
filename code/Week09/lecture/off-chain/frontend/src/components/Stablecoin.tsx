import { AppStateContext } from "@/pages/_app";
import {
    findUTxO,
    safeStringToBigInt,
    signAndSubmitTx,
} from "@/utilities/utilities";
import { getAddressDetails } from "lucid-cardano";
import { Data } from "lucid-cardano";
import { useContext, useState } from "react";

const CollateralDatum = Data.Object({   // Define the datatype of the collateral datum
    colMintingPolicyId: Data.Bytes(),
    colOwner: Data.Bytes(),
    colStablecoinAmount: Data.Integer(),
});
type CollateralDatum = Data.Static<typeof CollateralDatum>;

const CollateralRedeemer = Data.Enum([
    Data.Literal("Redeem"),
    Data.Literal("Liquidate"),
]);
type CollateralRedeemer = Data.Static<typeof CollateralRedeemer>;

const MintRedeemer = Data.Enum([
    Data.Literal("Mint"),
    Data.Literal("Burn"),
    Data.Literal("Liquidate"),
]);
type MintRedeemer = Data.Static<typeof MintRedeemer>;

export default function Stablecoin() {
    const { appState, setAppState } = useContext(AppStateContext);
    const {
        lucid,
        wAddr,
        minPercent,
        oracleWithNftUTxO,
        scAssetClassHex,
        scPolicyIdHex,
        collateralScript,
        collateralRefScrUTxO,
        collateralRefScrUTxORef,
        collateralToUnlockUTxO,
        collateralToUnlockUTxORef,
        mintingPolRefScrUTxO,
        mintingPolRefScrUTxORef,
    } = appState;
    const [amountToMint, setAmountToMint] = useState(10n);
    const [amountToBurnOrLiq, setAmountToBurnOrLiq] = useState(10n);
    const [burnOrLiq, setBurnOrLiq] = useState(true); // Burn = true
    const [collValueToLock, setCollValueToLock] = useState(15n);

    ///////////////////////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////// HELPER FUNCTIONS ///////////////////////////////////////////

    const updateMintingAndCollValues = (r: string) => {
        const scAmo = BigInt(Number(r));
        if (Number.isNaN(scAmo)) return;
        setAmountToMint(scAmo);
        if (!minPercent) return;
        const minColl = (Number(r) * minPercent) / 100;
        if (Number.isNaN(minColl)) return;
        setCollValueToLock(BigInt(minColl));
    };

    const getReferenceUTxOs = async () => {
        if (!lucid || !collateralRefScrUTxORef || !mintingPolRefScrUTxORef)
            return;

        const collRefScrUTxO = await findUTxO(lucid, collateralRefScrUTxORef);
        const mpRefScrUTxO = await findUTxO(lucid, mintingPolRefScrUTxORef);

        console.log("UTxOs: ", collRefScrUTxO, mpRefScrUTxO);
        setAppState({
            ...appState,
            collateralRefScrUTxO: collRefScrUTxO,
            mintingPolRefScrUTxO: mpRefScrUTxO,
        });
    };

    const getColateralUTxOToUnlock = async () => {
        if (
            !lucid ||
            !oracleWithNftUTxO ||
            !collateralRefScrUTxO ||
            !mintingPolRefScrUTxO ||
            !collateralToUnlockUTxORef
        )
            return;
        const colToUnlockUTxO = await findUTxO(
            lucid,
            collateralToUnlockUTxORef
        );
        console.log("Collateral to unlock UTxOs: ", colToUnlockUTxO);
        setAppState({
            ...appState,
            collateralToUnlockUTxO: colToUnlockUTxO,
        });
    };

    ///////////////////////////////////////////////////////////////////////////////////////////////
    //////////////////////////////////// MINT STABLECOINS /////////////////////////////////////////

    const mintSC = async () => {
        console.log("mintSC -> appState: ", appState);
        if (
            wAddr &&
            lucid &&
            oracleWithNftUTxO &&
            scAssetClassHex &&
            collateralRefScrUTxO &&
            mintingPolRefScrUTxO &&
            scPolicyIdHex &&
            amountToMint > 0n
        ) {
            const pkh: string = getAddressDetails(wAddr).paymentCredential?.hash || ""; // get current address's PubKeyHash

            const collDatum: CollateralDatum = {    // Define the datum we add to our collateral UTxO ... this is for lucid
                colMintingPolicyId: scPolicyIdHex,
                colOwner: pkh,
                colStablecoinAmount: amountToMint,
            };

            const collateralAddr = lucid.utils.validatorToAddress(collateralScript);    // get the address of the Collateral script to send our collateral to

            const tx = await lucid!
                .newTx()
                .readFrom([oracleWithNftUTxO, mintingPolRefScrUTxO])    // We explicitly read the UTxO containing the Oracle
                .payToContract(
                    collateralAddr,
                    {
                        inline: Data.to<CollateralDatum>(
                            collDatum,          // Our Collateral datum
                            CollateralDatum     // The data type of the Collateral datum
                        ),
                    },
                    { lovelace: collValueToLock * 1000000n }    // We get the 'collValueToLock' from the UI and convert it to lovelace
                )
                .mintAssets(                                    // We mint the stablecoin in the same txn
                    { [scAssetClassHex]: amountToMint },            // [AssetClass]: amount
                    Data.to<MintRedeemer>("Mint", MintRedeemer)     // "Value", datatype
                )
                .addSignerKey(pkh)
                .complete();

            await signAndSubmitTx(tx);
        } else {
            alert("Please, provide the reference scripts!");
        }
    };

    ///////////////////////////////////////////////////////////////////////////////////////////////
    ////////////////////////////// BURN / LIQUIDATE STABLECOINS ///////////////////////////////////

    const burnOrLiqSC = async () => {
        console.log("burnSC -> appState: ", appState);
        if (
            wAddr &&
            lucid &&
            scPolicyIdHex &&
            scAssetClassHex &&
            oracleWithNftUTxO &&
            collateralRefScrUTxO &&
            mintingPolRefScrUTxO &&
            collateralToUnlockUTxO &&
            amountToBurnOrLiq > 0n
        ) {
            console.log(
                `{-amountToBurnOrLiq: ${-amountToBurnOrLiq}, burn/liq: ${burnOrLiq}}`
            );
            const pkh: string = getAddressDetails(wAddr).paymentCredential?.hash || ""; // get PubKeyHash of current wallet

            const colRed: CollateralRedeemer = burnOrLiq                    // Set the Collateral Validator redeemer based on the selected radio button
                ? "Redeem"
                : "Liquidate";
            const mpRed: MintRedeemer = burnOrLiq ? "Burn" : "Liquidate";   // Set the Stablecoin MintingPolicy redeemer based on the selected radio button

            const tx = await lucid!
                .newTx()
                .readFrom([
                    oracleWithNftUTxO,      // We explicitly read the UTxO with the NFT at the OracleValidator's address as refenence input (we don't consume it)
                    collateralRefScrUTxO,   // We explicitly read the UTxO containing the Collateral Validator as a reference script
                    mintingPolRefScrUTxO,   // We explicitly read the UTxO containing the Stablecoin MintingPolicy as a reference script
                ])
                .collectFrom(
                    [collateralToUnlockUTxO],   // We explicitly input the UTxO containing the Collateral we want to burn/liquidate
                    Data.to<CollateralRedeemer>(colRed, CollateralRedeemer)     // We provide the appropriate redeemer
                )
                .mintAssets(
                    { [scAssetClassHex]: -amountToBurnOrLiq },  // [AssetClass]: -amount    ... burn the stablecoins
                    Data.to<MintRedeemer>(mpRed, MintRedeemer)  // Redeemer for the Stablecoin MintingPolicy
                )
                .addSignerKey(pkh)                  // This is actually only used when Burning / Redeeming, not Liquidating
                .complete({ nativeUplc: false });// Balance the txn (the parameter is just a flag to change which compiler checks the validators... doesn't really matter)

            await signAndSubmitTx(tx);
        } else {
            alert("Please, provide the reference scripts!");
        }
    };

    ///////////////////////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////// UI /////////////////////////////////////////////////

    return (
        <div className="text-zinc-800 font-quicksand">
            <div className="shadow-[0_4px_0px_0px_rgba(0,0,0,0.25)] w-[664px] bg-zinc-50 border border-zinc-600 rounded-xl p-9 mb-5 ">
                <div className="">
                    <div className="flex flex-col mb-2">
                        <p>Collateral UTxO Ref:</p>
                        <input
                            className="py-1 px-2 border border-zinc-700 rounded"
                            type="string"
                            value={collateralRefScrUTxORef || ""}
                            onChange={(e) =>
                                setAppState({
                                    ...appState,
                                    collateralRefScrUTxORef: e.target.value,
                                })
                            }
                        />
                    </div>
                    <div className="flex flex-col mb-2">
                        <p>Stablecoin Minting Policy UTxO Ref:</p>
                        <input
                            className="py-1 px-2 border border-zinc-700 rounded"
                            type="string"
                            value={mintingPolRefScrUTxORef || ""}
                            onChange={(e) =>
                                setAppState({
                                    ...appState,
                                    mintingPolRefScrUTxORef: e.target.value,
                                })
                            }
                        />
                    </div>
                    <div className="flex flex-row mb-2 items-baseline">
                        <p>Stablecoins to mint (units):</p>
                        <input
                            className="w-16 py-1 px-2 ml-2 border border-zinc-700 rounded"
                            type="number"
                            value={Number(amountToMint)}
                            onChange={(e) =>
                                updateMintingAndCollValues(e.target.value)
                            }
                        />
                    </div>
                    <div className="flex flex-row mb-2 items-baseline">
                        <p>Collateral to lock (in ADA):</p>
                        <input
                            className="w-16 py-1 px-2 ml-3 border border-zinc-700 rounded"
                            type="number"
                            value={Number(collValueToLock)}
                            onChange={(e) => {
                                const coll = safeStringToBigInt(e.target.value);
                                if (!coll) return;
                                setCollValueToLock(coll);
                            }}
                        />
                    </div>
                </div>
                <div className="w-full flex flex-row justify-center gap-4 mt-2">
                    <button
                        onClick={getReferenceUTxOs}
                        disabled={
                            !lucid ||
                            !wAddr ||
                            !amountToMint ||
                            !collValueToLock
                        }
                        className="w-full rounded-lg p-3 text-zinc-50 bg-zinc-800 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)] disabled:active:translate-y-0 disabled:active:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:bg-zinc-200  disabled:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:text-zinc-600 font-quicksand font-bold active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)]"
                    >
                        {" "}
                        Get Reference Script UTxOs
                    </button>
                    <button
                        onClick={mintSC}
                        disabled={
                            !lucid ||
                            !wAddr ||
                            !amountToMint ||
                            !collValueToLock ||
                            !oracleWithNftUTxO ||
                            !scAssetClassHex ||
                            !collateralRefScrUTxO ||
                            !mintingPolRefScrUTxO
                        }
                        className="w-full rounded-lg p-3 text-zinc-50 bg-zinc-800 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)] disabled:active:translate-y-0 disabled:active:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:bg-zinc-200  disabled:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:text-zinc-600 font-quicksand font-bold active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)]"
                    >
                        {" "}
                        Mint Stablecoins
                    </button>
                </div>
            </div>
            {/* SEGUNDA SECCION DE ACCIONES */}
            <div className="shadow-[0_4px_0px_0px_rgba(0,0,0,0.25)] w-[664px] bg-zinc-50 border border-zinc-600 rounded-xl p-9">
                <div className="flex flex-col mb-2">
                    <p>Collateral UTxO Ref to unlock:</p>
                    <input
                        className="py-1 px-2 border border-zinc-700 rounded"
                        type="string"
                        value={collateralToUnlockUTxORef || ""}
                        onChange={(e) =>
                            setAppState({
                                ...appState,
                                collateralToUnlockUTxORef: e.target.value,
                                collateralToUnlockUTxO: undefined,
                            })
                        }
                    />
                </div>

                <div className="flex flex-row mb-2 items-baseline">
                    <p>Stablecoins to burn/liquidate (units):</p>
                    <input
                        className="w-16 py-1 px-2 ml-2 border border-zinc-700 rounded"
                        type="number"
                        value={Number(amountToBurnOrLiq)}
                        onChange={(e) => {
                            const am = safeStringToBigInt(e.target.value);
                            if (!am) return;
                            setAmountToBurnOrLiq(am);
                        }}
                    />
                </div>
                <div className="flex flex-col">
                    <div className="flex flex-row mb-1 items-baseline">
                        <input
                            type="radio"
                            checked={burnOrLiq}
                            onChange={() => setBurnOrLiq(!burnOrLiq)}
                        />
                        <p className="ml-2">Burn</p>
                    </div>
                    <div className="flex flex-row mb-1 items-baseline">
                        <input
                            type="radio"
                            checked={!burnOrLiq}
                            onChange={() => setBurnOrLiq(!burnOrLiq)}
                        />
                        <p className="ml-2">Liquidate</p>
                    </div>
                </div>

                <div className="w-full flex flex-row justify-center gap-4 mt-2">
                    <button
                        onClick={getColateralUTxOToUnlock}
                        disabled={
                            !lucid ||
                            !wAddr ||
                            !amountToMint ||
                            !collValueToLock ||
                            !collateralToUnlockUTxORef
                        }
                        className="w-full rounded-lg p-3 text-zinc-50 bg-zinc-800 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)] disabled:active:translate-y-0 disabled:active:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:bg-zinc-200  disabled:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:text-zinc-600 font-quicksand font-bold active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)]"
                    >
                        {" "}
                        Get Collateral UTxO to unlock
                    </button>
                    <button
                        onClick={burnOrLiqSC}
                        disabled={
                            !lucid ||
                            !wAddr ||
                            !amountToMint ||
                            !collValueToLock ||
                            !oracleWithNftUTxO ||
                            !scAssetClassHex ||
                            !collateralRefScrUTxO ||
                            !mintingPolRefScrUTxO ||
                            !collateralToUnlockUTxO
                        }
                        className="w-full rounded-lg p-3 text-zinc-50 bg-zinc-800 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)] disabled:active:translate-y-0 disabled:active:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:bg-zinc-200  disabled:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:text-zinc-600 font-quicksand font-bold active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)]"
                    >
                        {" "}
                        Burn/Liquidate Stablecoins
                    </button>
                </div>
            </div>
        </div>
    );
}

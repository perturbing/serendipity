import * as L from "https://deno.land/x/lucid@0.10.6/mod.ts";
import validators from "./validators.json" assert { type: "json" };
import * as Types from "./src/types.ts";
import * as VRF from "./src/vrf.ts";
import { secretSeeds } from "./seeds.ts";
import { parse } from "https://deno.land/std@0.184.0/flags/mod.ts";
import {
    stakedFTtkn,
    createRand,
    filterUTxOs,
    randAddress,
    requestRand,
    lockingAddress,
    initStakedValue,
    unstakeValue,
    stakeValue,
    burn
} from "./src/functions.ts";



const flags = parse(Deno.args, {
  number: ["validatorID", "stakeAmount","kupo","kupmios"],
  boolean: ["stake", "unstake","deploy","burn","mint","help","createRequest"]
});



if (flags.help) {
    const helpText = 'To use to node you can use the flag\n\
    --help\n\
    --validatorID\n\
    --stake\n\
    --stakeAmount\n\
    --unstake\n\
    --deploy\n\
    --burn\n\
    --mint\n\
    ';
    console.log(helpText)
    Deno.exit(0)
}

const lucid: L.Lucid = await L.Lucid.new(
    new L.Kupmios(
      "http://"+String(flags.kupmios)+":1442",
      "ws://"+String(flags.kupmios)+":1337",
    ),
    "Preview",
);

const utxoAtStakeScript: L.UTxO[] = await lucid.utxosAt(lockingAddress);

if (flags.deploy) {
    const txHash = await initStakedValue();
    console.log(txHash);
    await lucid.awaitTx(txHash);
    Deno.exit(0)
}

if (flags.burn) {
    const txHash = await burn();
    console.log(txHash);
    await lucid.awaitTx(txHash);
    Deno.exit(0)
}

if (flags.createRequest) {
    const txHash = await requestRand();
    console.log(txHash);
    await lucid.awaitTx(txHash);
    Deno.exit(0)
}

// do some checks on the input args given
if ("validatorID" in flags) {
    if (typeof flags.validatorID === 'number' && flags.validatorID >=0 && flags.validatorID < validators.length) {}
    else {
        console.log("please provide an in range integer in the '--validatorID' flag");
        Deno.exit(1);
    };
} else
{
    console.error("please set a valid validator id via the flag '--validatorID'");
    Deno.exit(1);
}

const id = flags.validatorID;
const publicKey = validators[id].publicKey

if (flags.stake) {
    if ("stakeAmount" in flags) {
        if (typeof flags.stakeAmount === 'number') {
            const ourDtm = L.toHex(L.C.hash_blake2b256(L.fromHex(L.Data.to(new L.Constr(0,[publicKey])) )));
            const ourStakeUTxO: L.UTxO[] = utxoAtStakeScript.filter((utxo) => utxo.datumHash == ourDtm && utxo.assets[stakedFTtkn] > 0n);
            if (ourStakeUTxO.length == 0) {
                const txHash = await stakeValue(id,BigInt(flags.stakeAmount));
                console.log(txHash);
                await lucid.awaitTx(txHash);
                Deno.exit(0)
            } else {
                console.log("validator already staked")
                Deno.exit(1)
            }
        }
        else {
            console.log("please provide the amount of stake in the '--stakeAmount' flag");
            Deno.exit(1);
        };
    } else
    {
        console.error("please provide the amount of stake in the '--stakeAmount' flag");
        Deno.exit(1);
    }
}

if (flags.unstake) {
    const txHash = await unstakeValue(id);
    console.log(txHash);
    await lucid.awaitTx(txHash);
    Deno.exit(0);
}

interface State {
    walletUTxOUnused: L.UTxO[];
    requestUTxOUnused: L.UTxO[];
    walletUTxOUsed: L.UTxO[];
    requestUTxOUsed: L.UTxO[];
}

class SharedState {
    private state: State;
  
    constructor(initialState: State) {
      this.state = initialState;
    }
  
    getState(): State {
      return this.state;
    }
  
    setState(newState: State): void {
      this.state = newState;
    }
}


lucid.selectWalletFromSeed(secretSeeds[id+1]);
const sharedState = new SharedState({
    walletUTxOUnused: await lucid.wallet.getUtxos(),
    requestUTxOUnused: await lucid.utxosAt(randAddress),
    walletUTxOUsed: [],
    requestUTxOUsed: [],
});

const ourDtm = L.toHex(L.C.hash_blake2b256(L.fromHex(L.Data.to(new L.Constr(0,[validators[id].publicKey])) )));
const ourStakeUTxO: L.UTxO[] = utxoAtStakeScript.filter((utxo) => utxo.datumHash == ourDtm && utxo.assets[stakedFTtkn] > 0n);
const stake: bigint = ourStakeUTxO[0].assets[stakedFTtkn];

function arrayDifference<T>(a: T[], b: T[]): T[] {
    return a.filter(x => !b.includes(x));
}

async function job1() {
    const state = sharedState.getState();

    // the utxo's at the wallet if validator
    const utxoAtWalletUnUsed: L.UTxO[] = state.walletUTxOUnused;
    const utxoAtWalletUsed: L.UTxO[] = state.walletUTxOUsed;
    const walletUTxOAvailable: L.UTxO[] = arrayDifference(utxoAtWalletUnUsed,utxoAtWalletUsed)

    // the utxo's that request randomness
    const utxoAtRandScriptUnused: L.UTxO[] = state.requestUTxOUnused;
    const utxoAtRandScriptUsed: L.UTxO[] = state.requestUTxOUsed;
    const utxoAtRandScriptAvailable: L.UTxO[] = arrayDifference(utxoAtRandScriptUnused,utxoAtRandScriptUsed);

    // the utxo's that request randomness for which we have a VRF low enough
    const filteredUTxOs: L.UTxO[] = await filterUTxOs(stake,utxoAtRandScriptAvailable,id);

    const slotnr = lucid.currentSlot()-50;
        
    if (walletUTxOAvailable && filteredUTxOs) {
        const walletUTxo: L.UTxO | undefined = walletUTxOAvailable.shift()
        const requestUtxo: L.UTxO | undefined = filteredUTxOs.shift()
            if (walletUTxo !== undefined && requestUtxo !== undefined) {
                const currentTimeInMilliseconds = Date.now();
                const currentTimeInSeconds = Math.floor(currentTimeInMilliseconds / 1000);
                const formattedTime = new Date(currentTimeInSeconds * 1000).toLocaleString();
                try {
                    const txHash = await createRand(slotnr,walletUTxo,requestUtxo,ourStakeUTxO[0],id)
                    console.log(formattedTime+" | "+"Running Validator "+id + " | stake: " + stake + "/255 (" + ((Number(stake) / Number(255n)) * 100).toFixed(2) + "%)" + " | " + "Minted randomness! TxHash: " + txHash)
                    utxoAtWalletUsed.push(walletUTxo)
                } catch (error) {
                    utxoAtWalletUsed.push(walletUTxo)
                }
                sharedState.setState({
                    walletUTxOUnused: walletUTxOAvailable,
                    requestUTxOUnused: utxoAtRandScriptAvailable.filter(utxo => utxo !== requestUtxo),
                    walletUTxOUsed: [...utxoAtWalletUsed, walletUTxo],
                    requestUTxOUsed: [...utxoAtRandScriptUsed,requestUtxo],
                });
            }
    }
}

async function job2() {
    // const sharedState = new SharedState({
    //     walletUTxOUnused: await lucid.wallet.getUtxos(),
    //     requestUTxOUnused: await lucid.utxosAt(randAddress),
    //     walletUTxOUsed: [],
    //     requestUTxOUsed: [],
    // });
    const state = sharedState.getState();

    const currentWalletUTxOUnused = await lucid.wallet.getUtxos();
    const currentRequestUTxOUnused = await lucid.utxosAt(randAddress);

    const newWalletUnused = arrayDifference(currentWalletUTxOUnused,state.walletUTxOUsed)
    const newRequestUnused = arrayDifference(currentRequestUTxOUnused,state.requestUTxOUsed)

    sharedState.setState({
        walletUTxOUnused: newWalletUnused,
        requestUTxOUnused: newRequestUnused,
        walletUTxOUsed: state.walletUTxOUsed,
        requestUTxOUsed: state.requestUTxOUsed,
    });
}

if (flags.mint) {
    while (true) {
        const currentTimeInMilliseconds = Date.now();
        const currentTimeInSeconds = Math.floor(currentTimeInMilliseconds / 1000);
        const formattedTime = new Date(currentTimeInSeconds * 1000).toLocaleString();

        const state = sharedState.getState();

        // check if vrf value is low enough an grab request
        job1();
        job2();

        
        console.log(formattedTime+" | "+"Running Validator "+id + " | stake: " + stake + "/255 (" + ((Number(stake) / Number(255n)) * 100).toFixed(2) + "%)" + " | available wallet UTxO's: " + state.walletUTxOUnused.length);
        await new Promise((resolve) => setTimeout(resolve, 1000));
    }
}

console.log("please provide more flags, use the flag help to see a list of the flags.")

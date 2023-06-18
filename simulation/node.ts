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
    unstake,
    stake,
    burn
} from "./src/functions.ts";

const lucid: L.Lucid = await L.Lucid.new(
    new L.Kupmios(
      "http://127.0.0.3:1442",
      "ws://127.0.0.3:1337",
    ),
    "Preview",
);

const utxoAtStakeScript: L.UTxO[] = await lucid.utxosAt(lockingAddress);

const flags = parse(Deno.args, {
  number: ["validatorID", "stakeAmount"],
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
                const txHash = await stake(id,BigInt(flags.stakeAmount));
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
    const txHash = await unstake(id);
    console.log(txHash);
    await lucid.awaitTx(txHash);
    Deno.exit(0);
}

if (flags.mint) {
    while (true) {
        const currentTimeInMilliseconds = Date.now();
        const currentTimeInSeconds = Math.floor(currentTimeInMilliseconds / 1000);
        const formattedTime = new Date(currentTimeInSeconds * 1000).toLocaleString();

        lucid.selectWalletFromSeed(secretSeeds[id+1]);
        const ourDtm = L.toHex(L.C.hash_blake2b256(L.fromHex(L.Data.to(new L.Constr(0,[validators[id].publicKey])) )));
        const ourStakeUTxO: L.UTxO[] = utxoAtStakeScript.filter((utxo) => utxo.datumHash == ourDtm && utxo.assets[stakedFTtkn] > 0n);
        // The stake of validator
        const stake: bigint = ourStakeUTxO[0].assets[stakedFTtkn];
        // the utxo's at the wallet if validator
        const utxoAtWallet: L.UTxO[] = await lucid.wallet.getUtxos();

        // the utxo's that request randomness
        const utxoAtRandScript: L.UTxO[] = await lucid.utxosAt(randAddress);
        const filteredUTxOs: L.UTxO[] = await filterUTxOs(stake,utxoAtRandScript,id);

        const slotnr = lucid.currentSlot()-50;
        
        if (filteredUTxOs && filteredUTxOs.length > 0) {
            for (let i = 0; i < filterUTxOs.length; i++) {
                const txHash = await createRand(slotnr,utxoAtWallet[i],filteredUTxOs[i],ourStakeUTxO[0],id)
                console.log(formattedTime+" | "+"Running Validator "+id + " | stake: " + stake + "/255 (" + ((Number(stake) / Number(255n)) * 100).toFixed(2) + "%)" + " | " + "Minted randomness! TxHash: " + txHash)
                lucid.awaitTx(txHash)
        }}
        console.log(formattedTime+" | "+"Running Validator "+id + " | stake: " + stake + "/255 (" + ((Number(stake) / Number(255n)) * 100).toFixed(2) + "%)" + " | " + utxoAtRandScript.length + " requests currently");
        await new Promise((resolve) => setTimeout(resolve, 1000));
    }
}

console.log("please provide more flags, use the flag help to see a list of the flags.")

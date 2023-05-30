import * as L from "https://deno.land/x/lucid@0.10.5/mod.ts";
import {vrf_verify, vrf_proof, vrf_key_generate } from "./vrf.ts"
import * as Types from "./types.ts";
import { secretSeed } from "./seed.ts";

const lucid: L.Lucid = await L.Lucid.new(
    new L.Kupmios(
      "http://127.0.0.3:1442",
      "ws://127.0.0.3:1337",
    ),
    "Preview",
);

// set wallet
lucid.selectWalletFromSeed(secretSeed);
const addr: L.Address = await lucid.wallet.address();
const pkh: string = L.getAddressDetails(addr).paymentCredential?.hash;

// a function that reads an unparametrized plutus script as a file location.
async function readScript(name: string): Promise<L.MintingPolicy> {
    const validator = JSON.parse(await Deno.readTextFile("assets/"+ name))
    return {
      type: "PlutusV2",
      script: validator.cborHex
    }
}

// reading the VRF test script.
const lockVRFScript: L.SpendingValidator = await readScript("test.plutus");
const lockingAddress: L.Address = lucid.utils.validatorToAddress(lockVRFScript);

async function initState(): Promise<L.TxHash> {
    const tx = await lucid
      .newTx()
      .payToContract(lockingAddress, {inline: L.Data.to("00")},{lovelace: 10000000n})
      .complete();
    const signedTx = await tx.sign().complete();
   
    return signedTx.submit();
}

// the redeemer type of a tuple of Proofs.
const Redeemer = L.Data.Object({
    output: Types.Output,
    pubkey: Types.PubKey,
    proof:  Types.Proof
});
type Redeemer = L.Data.Static<typeof Redeemer>;

// an example of how to use these primitives
const [vrfPriv,vrfPub] = vrf_key_generate()

async function unlockState() {
    const utxoAtScript: L.UTxO[] = await lucid.utxosAt(lockingAddress);
  
    const time = Date.now();
    if (utxoAtScript && utxoAtScript.length > 0) {
      const firstUtxo = utxoAtScript[0];
      const datumOfFirstUtxo = firstUtxo.datum?firstUtxo.datum:"00";
      const input: Types.Input = {input: firstUtxo.txHash + L.Data.from(datumOfFirstUtxo)};
      const [output,proof] = await vrf_proof(input,vrfPriv);
      const pubKey: Types.PubKey = {pubkey: vrfPub }
      const testRedeemer = {
        output: output,
        pubkey: pubKey,
        proof: proof
      };
      const tx = await lucid
      .newTx()
      .collectFrom([firstUtxo], L.Data.to<Redeemer>(testRedeemer,Redeemer))
      .validFrom(time-20000)
      .validTo(time+1000000)
      .attachSpendingValidator(lockVRFScript)
      .complete();
    const signedTx = await tx.sign().complete();
   
    return signedTx.submit();
    }
    else return "No UTxO's found that can be burned"
}

//console.log(await initState());
//const txHash = await unlockState();
//console.log(txHash);
//console.log(await lucid.awaitTx(txHash))
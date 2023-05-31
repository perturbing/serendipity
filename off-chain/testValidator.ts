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

const freePolicyScript: L.MintingPolicy = await readScript("free-policy.plutus");
const freePolicy: L.PolicyId = lucid.utils.mintingPolicyToId(freePolicyScript);
console.log(freePolicy)

// reading the VRF test script.
const Params = L.Data.Tuple([Types.CurrencySymbol]);
type Params = L.Data.Static<typeof Params>;
const currencySymbol: Types.CurrencySymbol = freePolicy;
const params: Params = [currencySymbol];
async function readVRFScript(): Promise<L.SpendingValidator> {
  const script = JSON.parse(await Deno.readTextFile("assets/test.plutus"))
  return {
    type: "PlutusV2",
    script: L.applyParamsToScript<Params>(script.cborHex,params,Params)
  }
}
const lockVRFScript: L.SpendingValidator = await readVRFScript();
const lockingAddress: L.Address = lucid.utils.validatorToAddress(lockVRFScript);
console.log(lockingAddress)

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
//const [vrfPriv,vrfPub] = vrf_key_generate()
const vrfPriv = "9358f23af1af730d61ac747514981172566ef33aa3850f4f2a4b8656d8847247";
const vrfPub = "02c2c925377b60a088c57b54c2730a9fd0e6f32f35cbd1068f08a4ac76b6fc5ec6";

async function unlockState() {
    const utxoAtScript: L.UTxO[] = await lucid.utxosAt(lockingAddress);
  
    const time = Date.now()
    if (utxoAtScript && utxoAtScript.length > 0) {
      const firstUtxo = utxoAtScript[0];
      const datumOfFirstUtxo = firstUtxo.datum?firstUtxo.datum:"00";
      const input: Types.Input = {input: firstUtxo.txHash + L.Data.from(datumOfFirstUtxo)+"0"+lucid.utils.slotToUnixTime(lucid.utils.unixTimeToSlot(time-60*1000)).toString(16)}; // use padStart instead of "0"+slotHexString
      const [output,proof] = await vrf_proof(input,vrfPriv);
      const pubKey: Types.PubKey = {pubkey: vrfPub }
      const testRedeemer = {
        output: output,
        pubkey: pubKey,
        proof: proof
      };
      console.log(output)
      const tx = await lucid
      .newTx()
      .collectFrom([firstUtxo], L.Data.to<Redeemer>(testRedeemer,Redeemer))
      .validFrom(time-60*1000)
      .validTo(time+100*1000)
      .attachSpendingValidator(lockVRFScript)
      .complete();
    const signedTx = await tx.sign().complete();
   
    return signedTx.submit();
    }
    else return "No UTxO's found that can be burned"
}

//const txHash = await initState();
const txHash = await unlockState();
console.log(txHash);
console.log(await lucid.awaitTx(txHash))

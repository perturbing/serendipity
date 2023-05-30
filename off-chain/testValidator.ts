import * as L from "https://deno.land/x/lucid@0.10.5/mod.ts";
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
const pkh: string = L.getAddressDetails(addr).paymentCredential.hash;

// this function expects a hex string as its input.
function blake2b_256(input:string) {
    const bin = L.fromHex(input)
    return L.toHex(L.C.hash_blake2b256(bin))
}

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
console.log(lockingAddress)

async function initState(): Promise<L.TxHash> {
    const tx = await lucid
      .newTx()
      .payToContract(lockingAddress, {inline: L.Data.to("00")},{lovelace: 10000000n})
      .complete();
    const signedTx = await tx.sign().complete();
   
    return signedTx.submit();
}

const Redeemer = L.Data.Object({
    firstProof: Types.Proof,
    secondProof: Types.Proof
});
type Redeemer = L.Data.Static<typeof Redeemer>;


const testZKProof: Types.ZKProof = {c: "00",s:"00"};
const testGammma: Types.Gamma = {gamma: "00"};
const testProof: Types.Proof = {gamma: testGammma, zkproof: testZKProof};
const testRedeemer: Redeemer = {firstProof: testProof, secondProof: testProof};

async function unlockState(txHash:string) {
    const utxoAtScript: L.UTxO[] = await lucid.utxosAt(lockingAddress);
    const ourUTxO: L.UTxO[] = utxoAtScript.filter((utxo) => utxo.txHash == txHash);
  
    const time = Date.now()
    if (ourUTxO && ourUTxO.length > 0) {
      const tx = await lucid
      .newTx()
      .collectFrom(ourUTxO, L.Data.to<Redeemer>(testRedeemer,Redeemer))
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
console.log(await unlockState("504f44858c59b60a31618c5a2089723d8da68825ac773493df9460faa0d3971e"))
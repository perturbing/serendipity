import * as L from "https://deno.land/x/lucid@0.10.6/mod.ts";
import { secretSeeds } from "../seeds.ts";
import validators from "../validators.json" assert { type: "json" };
import * as Types from "./types.ts";
import * as VRF from "./vrf.ts";

const lucid: L.Lucid = await L.Lucid.new(
    new L.Kupmios(
      "http://127.0.0.3:1442",
      "ws://127.0.0.3:1337",
    ),
    "Preview",
);

export function intToUint8Array(num: number): Uint8Array {
    if (num === 0) {
      return new Uint8Array([0]);
    }
    
    let byteCount = Math.ceil(Math.log2(num + 1) / 8); // calculate byte count
    
    let arr = new Uint8Array(byteCount);
    for(let i = byteCount - 1; i >= 0; i--) {
      arr[i] = num & 0xFF;
      num >>= 8;
    }
  
    return arr;
}

function toAddress(address: Types.Address): L.Address {
    const paymentCredential = (() => {
      if ("PubKeyCredential" in address.addressCredential) {
        return lucid.utils.keyHashToCredential(
          address.addressCredential.PubKeyCredential[0],
        );
      } else {
        return lucid.utils.scriptHashToCredential(
          address.addressCredential.ScriptCredential[0],
        );
      }
    })();
    const stakeCredential = (() => {
      if (!address.addressStakingCredential) return undefined;
      if ("StakingHash" in address.addressStakingCredential) {
        if ("PubKeyCredential" in address.addressStakingCredential.StakingHash[0]) {
          return lucid.utils.keyHashToCredential(
            address.addressStakingCredential.StakingHash[0].PubKeyCredential[0],
          );
        } else {
          return lucid.utils.scriptHashToCredential(
            address.addressStakingCredential.StakingHash[0].ScriptCredential[0],
          );
        }
      } else {
        return undefined;
      }
    })();
    return lucid.utils.credentialToAddress(paymentCredential, stakeCredential);
}

lucid.selectWalletFromSeed(secretSeeds[0]);

const TxID = L.Data.Bytes();
type TxID = L.Data.Static<typeof TxID>;

const Ix = L.Data.Integer();
type Ix = L.Data.Static<typeof Ix>;

const TokenName = L.Data.Bytes();
type TokenName = L.Data.Static<typeof TokenName>;

const txID: TxID = "c1738dc007f95750e01fb241520add7d5c016fc90ad1c27d7d41489d958327c0";
const ix: Ix = 1n;
const tokenName: TokenName = L.fromText("Staked Token");

// import NFT minting policy and apply above parameters
const NFTParams = L.Data.Tuple([TxID, Ix, TokenName]);
type NFTParams = L.Data.Static<typeof NFTParams>;
const nftParameters: NFTParams = [txID, ix, tokenName];
async function readStakeFTPolicy(): Promise<L.MintingPolicy> {
    const script = JSON.parse(
        await Deno.readTextFile("assets/stake-nft-policy.plutus")
    );
    return {
        type: "PlutusV2",
        script: L.applyParamsToScript<NFTParams>(
            script.cborHex,
            nftParameters,
            NFTParams
        ),
    };
}
export const mintingStakedFTScript: L.MintingPolicy = await readStakeFTPolicy();
export const policyStakedFT: L.PolicyId = lucid.utils.mintingPolicyToId(mintingStakedFTScript);
export const stakedFTtkn: L.Unit = L.toUnit(policyStakedFT,tokenName)

// a helper function that reads an unparametrized plutus script
async function readScript(name: string): Promise<L.SpendingValidator> {
    const validator = JSON.parse(await Deno.readTextFile("assets/" + name));
    return {
        type: "PlutusV2",
        script: validator.cborHex,
    };
}
// a helper function that reads an unparametrized plutus policy
async function readPolicy(name: string): Promise<L.MintingPolicy> {
    const validator = JSON.parse(await Deno.readTextFile("assets/" + name));
    return {
        type: "PlutusV2",
        script: validator.cborHex,
    };
}

// import the locking validator (this locks the reference token of the PPP Certificate NFT)
export const lockingValidator: L.SpendingValidator = await readScript("stake-validator.plutus");
export const lockingAddress: L.Address = lucid.utils.validatorToAddress(lockingValidator);

export const alwaysFreeValidator: L.SpendingValidator = await readScript("free-validator.plutus");
export const alwaysFreeAddress: L.Address = lucid.utils.validatorToAddress(alwaysFreeValidator);

export const alwaysFreeMint: L.MintingPolicy = await readPolicy("free-validator.plutus");
export const alwaysFreePol: L.PolicyId = lucid.utils.mintingPolicyToId(alwaysFreeMint);

// import NFT minting policy and apply above parameters
const RandParams = L.Data.Tuple([Types.CurrencySymbol]);
type RandParams = L.Data.Static<typeof RandParams>;
const randParameters: RandParams = [policyStakedFT];
async function readRandValidator(): Promise<L.SpendingValidator> {
    const script = JSON.parse(
        await Deno.readTextFile("assets/rand-validator.plutus")
    );
    return {
        type: "PlutusV2",
        script: L.applyParamsToScript<RandParams>(
            script.cborHex,
            randParameters,
            RandParams
        ),
    };
}
export const randValidator: L.SpendingValidator = await readRandValidator();
export const randAddress: L.Address = lucid.utils.validatorToAddress(randValidator);

export async function initStakedValue(): Promise<L.TxHash> {
    const utxo = await lucid.utxosByOutRef([{txHash:txID, outputIndex: Number(ix)}])
    const tx = lucid.newTx();
    for (let i = 0; i < 5; i++) {
        const dtm: Types.PubKey = {pubkey: validators[i].publicKey}
        tx.payToContract(
            lockingAddress,
            L.Data.to<Types.PubKey>(dtm,Types.PubKey),
            {[stakedFTtkn]: 51n})
    }
    tx.collectFrom(utxo)
      .mintAssets({ [stakedFTtkn]: 255n}, L.Data.void())
      .attachMintingPolicy(mintingStakedFTScript)
    const finalTx = await tx.complete();
    const signedTx = await finalTx.sign().complete();
   
    return signedTx.submit();
}

const Redeemer = L.Data.Object({output: Types.Output, proof: Types.Proof});
type Redeemer = L.Data.Static<typeof Redeemer>;

export async function unstakeValue(id:number) {
    lucid.selectWalletFromSeed(secretSeeds[id+1]);
    const utxoAtScript: L.UTxO[] = await lucid.utxosAt(lockingAddress);
    const ourDtm = L.toHex(L.C.hash_blake2b256(L.fromHex(L.Data.to(new L.Constr(0,[validators[id].publicKey])) )));
    const ourUTxO: L.UTxO[] = utxoAtScript.filter((utxo) => utxo.datumHash == ourDtm && utxo.assets[stakedFTtkn] > 0n);
    
    if (ourUTxO && ourUTxO.length > 0) {
        const input: Types.Input = {input: ourUTxO[0].txHash + validators[id].publicKey};
        const [output,proof] = await VRF.vrf_proof(input, validators[id].privateKey);
        
        const redeemer: Redeemer = {output: output , proof: proof};
        const time = Date.now();
        const tx = await lucid
        .newTx()
        .collectFrom(ourUTxO, L.Data.to<Redeemer>(redeemer,Redeemer))
        .attachSpendingValidator(lockingValidator)
        .validFrom(time-60*1000)
        .validTo(time+100*1000)
        .complete()
        const signedTx = await tx.sign().complete();
        return signedTx.submit();
    }
    else return "No UTxO's found that can be used"
}

export async function stakeValue(id:number, stake:bigint): Promise<L.TxHash> {
    lucid.selectWalletFromSeed(secretSeeds[id+1]);
    const tx = lucid.newTx();
    const dtm: Types.PubKey = {pubkey: validators[id].publicKey}
    tx.payToContract(
        lockingAddress,
        L.Data.to<Types.PubKey>(dtm,Types.PubKey),
        {[stakedFTtkn]: stake})
    const finalTx = await tx.complete();
    const signedTx = await finalTx.sign().complete();
   
    return signedTx.submit();
}

// given that you have 255 tokens that can be stakes
// you can burn them all at once.
export async function burn(): Promise<L.TxHash> {
    const stakedFTtkn: L.Unit = L.toUnit(policyStakedFT,tokenName);
    const tx = lucid
      .newTx()
      .mintAssets({ [stakedFTtkn]: -255n}, L.Data.to(new L.Constr(1,[])))
      .attachMintingPolicy(mintingStakedFTScript)
    const finalTx = await tx.complete();
    const signedTx = await finalTx.sign().complete();
   
    return signedTx.submit();
}

const ReqDatum = L.Data.Object({requester: Types.PubKeyHash, address: Types.Address});
type ReqDatum = L.Data.Static<typeof ReqDatum>;

// currently this implements no target address for the randomness
export async function requestRand(): Promise<L.TxHash> {
    const addr: L.Address = await lucid.wallet.address();
    const pkh: string = L.getAddressDetails(addr).paymentCredential.hash;
    const alwaysFreeAddressScriptHash = L.getAddressDetails(alwaysFreeAddress).paymentCredential.hash;
    const addrAlwaysTrue: Types.Address = {addressCredential: { ScriptCredential: [alwaysFreeAddressScriptHash] }, addressStakingCredential: null};
    const reqDatum: ReqDatum = {requester: pkh, address: addrAlwaysTrue};
    const tx = lucid
      .newTx()
      .payToContract(
        randAddress,
        L.Data.to<ReqDatum>(reqDatum,ReqDatum),
        {lovelace: 15000000n}
      )
    const finalTx = await tx.complete();
    const signedTx = await finalTx.sign().complete();
   
    return signedTx.submit();
}


export async function requestRandCancel(): Promise<L.TxHash> {
    const addr: L.Address = await lucid.wallet.address();
    const pkh: string = L.getAddressDetails(addr).paymentCredential.hash;
    const alwaysFreeAddressScriptHash = L.getAddressDetails(alwaysFreeAddress).paymentCredential.hash;
    const addrAlwaysTrue: Types.Address = {addressCredential: { ScriptCredential: [alwaysFreeAddressScriptHash] }, addressStakingCredential: null};
    const reqDatum: ReqDatum = {requester: pkh, address: addrAlwaysTrue}
    const ourDtm = L.toHex(L.C.hash_blake2b256(L.fromHex(L.Data.to<ReqDatum>(reqDatum,ReqDatum) )));
    const utxoAtReqScript: L.UTxO[] = await lucid.utxosAt(randAddress);
    const ourRequests: L.UTxO[] = utxoAtReqScript.filter((utxo) => utxo.datumHash == ourDtm);

    if (ourRequests && ourRequests.length > 0) {
        const tx = await lucid
        .newTx()
        .collectFrom(ourRequests, L.Data.to(new L.Constr(0,[])))
        .attachSpendingValidator(randValidator)
        .addSignerKey(pkh)
        .complete();
      const signedTx = await tx.sign().complete();
     
      return signedTx.submit();
      }
      else return "No UTxO's found that can be burned"

}

const ReqRedeemer = L.Data.Enum([
    L.Data.Literal("Cancel"),
    L.Data.Object({ Mint: L.Data.Tuple([Redeemer]) })
]);
type ReqRedeemer = L.Data.Static<typeof ReqRedeemer>;

// spend requested randomness UTxO with a given UTxO in a wallet of validator given by id.
export async function createRand(slotnr:number,valUtxo:L.UTxO,reqUtxo:L.UTxO,stakeUtxo:L.UTxO,id:number): Promise<L.TxHash> {
    lucid.selectWalletFromSeed(secretSeeds[id+1]);
    const time = lucid.utils.slotToUnixTime(slotnr)
    const input: Types.Input = {input: reqUtxo.txHash +  L.toHex(intToUint8Array(time / 100000))};
    const [output,proof] = await VRF.vrf_proof(input, validators[id].privateKey);
    
    const dtm = await lucid.provider.getDatum(reqUtxo.datumHash);
    const datum: ReqDatum = L.Data.from<ReqDatum>(dtm,ReqDatum);
    const reqAddr: L.Address = toAddress(datum.address);
    const newValue = {...reqUtxo.assets, lovelace: reqUtxo.assets.lovelace - 10000000n}

    const red: Redeemer = {output: output , proof: proof};
    const redeemer: ReqRedeemer = {Mint: [red]}
    const vrfProofTkn: L.Unit = L.toUnit(alwaysFreePol,L.fromText("Oracle counter"));
    const tx = lucid
      .newTx()
      .collectFrom([valUtxo])
      .collectFrom([reqUtxo], L.Data.to<ReqRedeemer>(redeemer,ReqRedeemer))
      .attachSpendingValidator(randValidator)
      .readFrom([stakeUtxo])
      .payToContract(
        reqAddr,
        L.Data.to( L.toHex(L.C.hash_blake2b256(L.fromHex(output.output))) ),
        newValue
      )
      // .mintAssets({[vrfProofTkn]:1n}, L.Data.void())       // 
      // .attachMintingPolicy(alwaysFreeMint)                 // Enable this for testing and keeping track of the stochastic nature of the POS protocol.
      .validFrom(time)
      .validTo(time+100*1000)
    const finalTx = await tx.complete({coinSelection: false});
    const signedTx = await finalTx.sign().complete();

    return signedTx.submit();
}

async function filterUTxOAsync(stake:bigint,utxo: L.UTxO,id:number): Promise<boolean> {
    const slotnr = lucid.currentSlot()-50;
    const time = lucid.utils.slotToUnixTime(slotnr);
    const input: Types.Input = {input: utxo.txHash +  L.toHex(intToUint8Array(time / (1000*100)))};
    const [output,_proof] = await VRF.vrf_proof(input, validators[id].privateKey);

    const currentTimeInMilliseconds = Date.now();
    const currentTimeInSeconds = Math.floor(currentTimeInMilliseconds / 1000);
    const formattedTime = new Date(currentTimeInSeconds * 1000).toLocaleString();
    console.log(formattedTime+" | "+"Running Validator "+id + " | stake: " + stake + "/255 (" + ((Number(stake) / Number(255n)) * 100).toFixed(2) + "%)" + " | " + "VRF value for UTxO ref \"" + utxo.txHash + "\" is " + BigInt("0x" + output.output.substring(0,2)))
    return (BigInt("0x" + output.output.substring(0,2)) < stake); 
}

export async function filterUTxOs(stake:bigint,utxos:L.UTxO[],id:number): Promise<L.UTxO[]> {
    const filteredObjects = await Promise.all(utxos.map(async (utxo) => {
      const shouldInclude = await filterUTxOAsync(stake,utxo,id);
      return shouldInclude ? utxo : null;
    }));
  
    return filteredObjects.filter((utxo) => utxo !== null) as L.UTxO[];
}
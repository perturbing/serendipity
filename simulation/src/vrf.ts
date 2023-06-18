import * as L from "https://deno.land/x/lucid@0.10.6/mod.ts";
import * as secp from "https://deno.land/x/secp256k1/mod.ts";
import * as Types from "./types.ts";

// this function expects a hex string as its input and returns a hex string.
export function blake2b_256(input:string) {
    const bin = L.fromHex(input)
    return L.toHex(L.C.hash_blake2b256(bin))
}

// this function expects a hex string as its input and returns a hex string.
export function sha_256(input:string) {
    return L.toHex(L.sha256(L.fromHex(input)))
}

// generate a new vrf key pair.
export function vrf_key_generate() {
    const privKey = secp.utils.randomPrivateKey();
    const pubKey = secp.getPublicKey(privKey);
    return [L.toHex(privKey), L.toHex(pubKey)];
}

// generate a vrf proof.
export async function vrf_proof(input:Types.Input,privKey:string): Promise<[Types.Output,Types.Proof]> {
    const pubKey = secp.getPublicKey(privKey);
    const gamma: Types.Gamma = { gamma: sha_256(input.input+L.toHex(pubKey)) };
    const signature = await secp.signAsync(gamma.gamma, privKey);
    const zkproof: Types.ZKProof = { zkproof: signature.toCompactHex() }
    const output: Types.Output = { output: blake2b_256(gamma.gamma + zkproof.zkproof) }
    const proof: Types.Proof = {gamma:gamma, zkproof:zkproof }
    return [output,proof]
}

// verify a vrf proof.
export function vrf_verify(input:Types.Input,output:Types.Output,pubKey:string,proof:Types.Proof): boolean {
    const sig = secp.Signature.fromCompact(proof.zkproof.zkproof);
    const msg = proof.gamma.gamma
    const checkGamma = sha_256(input.input+pubKey)
    const isValid = secp.verify(sig, msg, pubKey);
    const checkOutput = blake2b_256(msg+sig.toCompactHex())
    return isValid && checkOutput == output.output && checkGamma == msg
}

// // an example of how to use these primitives

// // generate a new vrf key pair.
// const [vrfPriv,vrfPub] = vrf_key_generate()

// // Set a test input
// const input:Types.Input = { input: L.fromText("greetings from noble")}

// // Generate a VRF output and proof given the above input under our keys
// const [output,proof] = await vrf_proof(input,vrfPriv);

// // Verify the above VRF ouput given the input, pubkey and proof.
// const isValid = vrf_verify(input,output,vrfPub,proof);

// console.log(isValid)
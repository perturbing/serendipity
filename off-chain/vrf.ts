import * as L from "https://deno.land/x/lucid@0.10.5/mod.ts";
import { sha256 } from "https://denopkg.com/chiefbiiko/sha256@v1.0.0/mod.ts"
import * as secp from "https://deno.land/x/secp256k1/mod.ts";
import * as Types from "./types.ts";

// this function expects a hex string as its input.
function blake2b_256(input:string) {
    const bin = L.fromHex(input)
    return L.toHex(L.C.hash_blake2b256(bin))
}

// this function expects a Uint8Array as its input.
function sha_256(input:Uint8Array) {
    return L.toHex(sha256(input))
}

export function vrf_key_generate() {
    const privKey = secp.utils.randomPrivateKey();
    const pubKey = secp.getPublicKey(privKey);
    return {
        PrivateKey: L.toHex(privKey),
        PublicKey: L.toHex(pubKey)
    }
}

// input and privkey in Hex.
export async function vrf_proof(input:Types.Input,privKey:string): Promise<[Types.Output,Types.Proof]> {
    const gamma: Types.Gamma = { gamma: sha_256(L.fromHex(input.input)) };
    const signature = await secp.signAsync(gamma.gamma, privKey);
    const zkproof: Types.ZKProof = { zkproof: signature.toCompactHex() }
    const output: Types.Output = { output: blake2b_256(gamma.gamma + zkproof.zkproof) }
    const proof: Types.Proof = {gamma:gamma, zkproof:zkproof }
    return [output,proof]
}

export function vrf_verify(output:Types.Output,proof:Types.Proof,pubKey:string): boolean {
    const sig = secp.Signature.fromCompact(proof.zkproof.zkproof);
    const msg = proof.gamma.gamma
    const isValid = secp.verify(sig, msg, pubKey);
    const checkOutput = blake2b_256(msg+sig.toCompactHex())
    return isValid && checkOutput == output.output
}

// an example of how to use these primitives
const privKey = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef";
const pubKey = L.toHex(secp.getPublicKey(privKey)); // the  33 bytes pubkey (034646ae5047316b4230d0086c8acec687f00b1cd9d1dc634f6cb358ac0a9a8fff)

const input:Types.Input = { input: L.fromText("greetings from noble")}

const [output,proof] = await vrf_proof(input,privKey);
console.log(vrf_verify(output,proof,pubKey))
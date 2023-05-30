import * as L from "https://deno.land/x/lucid@0.10.5/mod.ts";

export const Input = L.Data.Object({
    input : L.Data.Bytes()
});
export type Input = L.Data.Static<typeof Input>;

export const Output = L.Data.Object({
    output: L.Data.Bytes()
});
export type Output = L.Data.Static<typeof Output>;

export const PubKey = L.Data.Object({
    pubkey: L.Data.Bytes()
});
export type PubKey = L.Data.Static<typeof PubKey>;

export const Gamma = L.Data.Object({
    gamma: L.Data.Bytes()
});
export type Gamma = L.Data.Static<typeof Gamma>;

export const ZKProof = L.Data.Object({
    zkproof: L.Data.Bytes()
});
export type ZKProof = L.Data.Static<typeof ZKProof>;

export const Proof = L.Data.Object({
    gamma: Gamma,
    zkproof: ZKProof
  })
export type Proof = L.Data.Static<typeof Proof>

// The type of a CurrencySymbol with the constraint that it must be of the exact size of 28 bytes
export const CurrencySymbol = L.Data.Bytes({ minLength: 28, maxLength: 28 });
export type CurrencySymbol = L.Data.Static<typeof CurrencySymbol>;
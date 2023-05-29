import * as L from "https://deno.land/x/lucid@0.10.5/mod.ts";

export const Gamma = L.Data.Object({
    gamma: L.Data.Bytes()
});
export type Gamma = L.Data.Static<typeof Gamma>;

export const ZKProof = L.Data.Object({
    c: L.Data.Bytes(),
    s: L.Data.Bytes()
});
export type ZKProof = L.Data.Static<typeof ZKProof>;

export const Proof = L.Data.Object({
    gamma: Gamma,
    zkproof: ZKProof
  })
export type Proof = L.Data.Static<typeof Proof>
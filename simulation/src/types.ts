import * as L from "https://deno.land/x/lucid@0.10.6/mod.ts";

export const PubKeyHash = L.Data.Bytes({ minLength: 28, maxLength: 28 });
export type PubKeyHash = L.Data.Static<typeof PubKeyHash>;

export const ValidatorHash = L.Data.Bytes({ minLength: 28, maxLength: 28 });
export type ValidatorHash = L.Data.Static<typeof ValidatorHash>;

export const Credential = L.Data.Enum([
    L.Data.Object({ PubKeyCredential: L.Data.Tuple([PubKeyHash]) }),
    L.Data.Object({ ScriptCredential: L.Data.Tuple([ValidatorHash]) })
]);
export type Credential = L.Data.Static<typeof Credential>;

//In an address, a chain pointer refers to a point of the chain containing a stake key registration certificate. A point is identified by 3 coordinates:
// An absolute slot number
// A transaction index (within that slot)
// A (delegation) certificate index (within that transaction)
export const StakingPtr = L.Data.Object({
    slotNumber: L.Data.Integer(),
    transactionIndex: L.Data.Integer(),
    certificateIndex: L.Data.Integer()
});
export type StakingPtr = L.Data.Static<typeof StakingPtr>;

export const StakingCredential = L.Data.Enum([
    L.Data.Object({ StakingHash: L.Data.Tuple([Credential]) }),
    L.Data.Object({ StakingPtr: L.Data.Tuple([StakingPtr]) })
]);
export type StakingCredential = L.Data.Static<typeof StakingCredential>;

export const Address = L.Data.Object({
    addressCredential: Credential,
    addressStakingCredential: L.Data.Nullable(StakingCredential)
});
export type Address = L.Data.Static<typeof Address>;

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
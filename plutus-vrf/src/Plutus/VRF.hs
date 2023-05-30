{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Plutus.VRF where

import PlutusTx
import PlutusTx.Builtins
import PlutusTx.Prelude

-- | A type for representing a VRF input
newtype Input = Input BuiltinByteString
makeIsDataIndexed ''Input [('Input,0)]

-- | A type for representing a VRF output
newtype Output = Output BuiltinByteString
makeIsDataIndexed ''Output [('Output,0)]

-- | A type for representing a VRF Public key
newtype PubKey = PubKey BuiltinByteString
makeIsDataIndexed ''PubKey [('PubKey,0)]

-- | A type for representing Gamma in the VRF scheme of draft-irtf-cfrg-vrf-15.
newtype Gamma = Gamma BuiltinByteString
makeIsDataIndexed ''Gamma [('Gamma,0)]

-- | A type for representing a non interactive proof in the above 
-- standard (normally this contains c and s)
-- data ZKProof = ZKProof BuiltinByteString BuiltinByteString
newtype ZKProof = ZKProof BuiltinByteString
makeIsDataIndexed ''ZKProof [('ZKProof,0)]

-- | A type for representing a VRF Proof.
-- A proof = (gamma, c, s) where the VRF Output can be derived
-- from gamma, and c and s are part of a non interactive zero
-- knowledge proof that gamma and the public key 
data Proof = Proof Gamma ZKProof
makeIsDataIndexed ''Proof [('Proof,0)]

-- | This is currently not a proper VRF! It just emulates the behaviour of one 
-- in a insecure way. This implementation is insecure because the output is a hash
-- of the gamma concatenated with the a ECDSA signature, which is not uniformly distributed.
-- Also, we use ECDSA, which has malleable signatures, so the output is not unique.
-- 
-- In general these three securitry properties must hold for a VRF
-- 1: Pseudorandomness (the outputs look random for someone who does not have the secret key of pk)
-- 2: Uniqueness (for any pk and input, there is a unique output)
-- 3: Collision resistance (This must also hold for an adversiary that knows the private key of pk)
--
-- Replace this when CIP 381 is implemented in plutus. For a proper VRF consider the following gist
-- https://gist.github.com/perturbing/ebde137286944b30b1de2277cfaf1c5a
{-# INLINEABLE verifyVRF #-}
verifyVRF :: Input -> Output -> PubKey -> Proof -> Bool
-- verifyVRF (Input inBs) (Output outBs) (PubKey pk) _ = outBs == blake2b_256 (inBs <> pk)
verifyVRF (Input inBs) (Output outBs) (PubKey pk) (Proof (Gamma gammaBs) (ZKProof proofBs)) =
     verifyEcdsaSecp256k1Signature pk gammaBs proofBs && -- verify that proofBs is a valid signature of the msg hash gammaBs under pk.
     gammaBs == sha2_256 (inBs <> pk) &&                 -- verify that the msg that was signed, is inBs concatenated with the pk.
     outBs == blake2b_256 (gammaBs <> proofBs)           -- verify that the outBs is the blake2b hash of the signature and the msg hash
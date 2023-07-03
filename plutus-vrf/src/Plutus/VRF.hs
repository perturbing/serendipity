{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Plutus.VRF where

import PlutusTx           ( makeIsDataIndexed )
import PlutusTx.Builtins  ( BuiltinByteString, sha2_256, verifyEcdsaSecp256k1Signature,
                            blake2b_256 )
import PlutusTx.Prelude   ( Bool, (==), (&&), traceIfFalse, (<>) )

-- [General notes on this file]
-- This file contains the logic of the onchain Verifiable random function.
-- For reference see https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-vrf-15#name-elliptic-curve-vrf-ecvrf
-- Currently this VRF is not a real one due to the fact that the BLS primitives are not yet available for use.
-- An insecure replacement is made using a digital signature.

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
-- in a insecure way. This implementation is insecure because an ECDSA signature
-- is not unique (since it relies on a ephimiral key). This breaks property 2 below.
-- 
-- In general these three securitry properties must hold for a VRF
-- 1: Pseudorandomness (the outputs look random for someone who does not have the secret key of pk)
-- 2: Uniqueness (for any pk and input, there is a unique output)
-- 3: Collision resistance (This must also hold for an adversary that knows the private key of pk)
--
-- Replace this when CIP 381 is implemented in plutus. For a proper VRF consider the following gist
-- https://gist.github.com/perturbing/ebde137286944b30b1de2277cfaf1c5a
-- Or replace the ECDSA with a BLS signature (this preserves the uniqueness property).
{-# INLINEABLE verifyVRF #-}
verifyVRF :: Input -> Output -> PubKey -> Proof -> Bool
verifyVRF (Input inBs) (Output outBs) (PubKey pk) (Proof (Gamma gammaBs) (ZKProof proofBs)) =
     traceIfFalse "VRF-Fail: ECDSA" (verifyEcdsaSecp256k1Signature pk gammaBs proofBs) &&      -- verify that proofBs is a valid signature of the msg hash gammaBs under pk.
     traceIfFalse "VRF-Fail: msg not input hash" (gammaBs == sha2_256 (inBs <> pk)) &&         -- verify that the msg that was signed, is inBs concatenated with the pk.
     traceIfFalse "VRF-Fail: output not correct" (outBs == blake2b_256 (gammaBs <> proofBs))   -- verify that the outBs is the blake3b hash of the signature and the msg hash

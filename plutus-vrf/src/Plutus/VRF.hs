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

-- | A standin type for representing a non interactive proof in the above 
-- standard (normally this contains c and s)
-- now this is just a ECDSA over SECP256k1 of gamma under a public key
newtype ZKProof = ZKProof BuiltinByteString
makeIsDataIndexed ''ZKProof [('ZKProof,0)]

-- | A type for representing a VRF Proof.
-- A proof = (gamma, c, s) where the VRF Output can be derived
-- from gamma, and c and s are part of a non interactive zero
-- knowledge proof that gamma and the public key 
data Proof = Proof Gamma ZKProof
makeIsDataIndexed ''Proof [('Proof,0)]

-- | This is currently not a propper VRF! It just emulates the behaviour of one 
-- in a insecure way. This implementation is insecure since the output is a hash
-- of gamma, which is not uniformly distributed. Secondly, the outputs are predicable
-- given a public key. So this function is not even random!
{-# INLINEABLE verifyVRF #-}
verifyVRF :: Input -> Output -> PubKey -> Proof -> Bool
verifyVRF (Input inBs) (Output outBs) (PubKey pk) (Proof _ (ZKProof proofBs)) =
    verifyEcdsaSecp256k1Signature pk gamma proofBs && outBs == blake2b_256 gamma
    where
        gamma = blake2b_256 (inBs <> pk)

        

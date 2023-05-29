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
data ZKProof = ZKProof BuiltinByteString BuiltinByteString
makeIsDataIndexed ''ZKProof [('ZKProof,0)]

-- | A type for representing a VRF Proof.
-- A proof = (gamma, c, s) where the VRF Output can be derived
-- from gamma, and c and s are part of a non interactive zero
-- knowledge proof that gamma and the public key 
data Proof = Proof Gamma ZKProof
makeIsDataIndexed ''Proof [('Proof,0)]

-- | This is currently not a proper VRF! It just emulates the behaviour of one 
-- in a insecure way. This implementation is insecure because the output is a hash
-- of the input concatenated with the public key, which is not uniformly distributed.
-- Secondly, the outputs are predicable given a public key. So this function is not even random! 
-- Replace this when CIP 381 is implemented in plutus. For a proper VRF consider the following gist
-- https://gist.github.com/perturbing/ebde137286944b30b1de2277cfaf1c5a
{-# INLINEABLE verifyVRF #-}
verifyVRF :: Input -> Output -> PubKey -> Proof -> Bool
verifyVRF (Input inBs) (Output outBs) (PubKey pk) _ = outBs == blake2b_256 (inBs <> pk)

        

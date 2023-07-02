{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE NamedFieldPuns    #-}

module StakeValidator where

import Plutus.VRF                   ( verifyVRF, Proof, Input (Input), Output, PubKey (PubKey) )
import PlutusLedgerApi.V2.Contexts  ( ScriptContext, TxId (..) )
import PlutusTx.Prelude             ( Bool(..), error, ($), (<>) )
import PlutusTx                     ( BuiltinData, compile, CompiledCode, makeIsDataIndexed )
import Utilities                    ( writeCodeToFile, wrapValidator )
import Prelude                      ( IO )
import PlutusLedgerApi.V2           ( ScriptContext(..), TxOutRef, ScriptPurpose(..), TxOutRef(..) )

-- [General notes on this module]
-- This module includes the `stakeValidator`, a Plutus validator script responsible for governing the unlocking of staked value.
-- The datum of an output under this script includes a VRF PubKey, serving to associate and allocate the stake to a specific PubKey.
-- Future improvements could enable delegation of stake and prefent moving stake to quickly.

-- Auxiliary function and data type definitions ------------------

-- The 'Redeemer' structure for the validator contains a VRF output and proof, which are needed to unlock an output
-- secured by this script.
data Redeemer = Redeemer {
    output :: Output,
    proof  :: Proof
}
makeIsDataIndexed ''Redeemer [('Redeemer,0)]

-- The 'stakeValidator' function verifies that a value can be unlocked by checking a valid VRF proof 
-- for the PubKey in the datum. The input to the VRF is created by concatenating the transaction reference 
-- that created this output with the VRF public key.

{-# INLINABLE  stakeValidator #-}
stakeValidator :: PubKey -> Redeemer -> ScriptContext -> Bool
stakeValidator (PubKey pk) Redeemer{output,proof} ctx = checkVRF
    where
        checkVRF :: Bool
        checkVRF = verifyVRF (Input (ownRefBS <> pk )) output (PubKey pk) proof

        ownOutRef :: TxOutRef
        ownOutRef = getRef ctx
            where
                getRef :: ScriptContext -> TxOutRef
                getRef ScriptContext{scriptContextPurpose= Spending ref} = ref
                getRef _ = error ()

        ownRefBS = getTxId $ txOutRefId ownOutRef

{-# INLINABLE  mkWrappedStakeValidator #-}
mkWrappedStakeValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedStakeValidator = wrapValidator stakeValidator

stakeValidatorCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
stakeValidatorCode = $$(compile [|| mkWrappedStakeValidator ||])

saveStakeValidator :: IO ()
saveStakeValidator = writeCodeToFile "../assets/stake-validator.plutus" stakeValidatorCode
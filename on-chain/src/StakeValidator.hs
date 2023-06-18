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

-- [General notes on this file]
-- This file contains one plutus validator, namely the logic of the script that holds someone's staked value.
-- Together with this staked value locked, the datum of an output contains a VRF PubKey hash that is
-- used to identify and assign the stake to a PubKey.
--
-- This validator could be upgraded to allow for deligation of stake.

-- The 'Redeemer' of the validator that contains a VRF output and proof to unlock an output
-- at this script below. These two values are associated with input described below.
data Redeemer = Redeemer {
    output :: Output,
    proof  :: Proof
}
makeIsDataIndexed ''Redeemer [('Redeemer,0)]

-- The 'stakeValidator', value here can be unlocked by a valid VRF proof of this PubKey in the datum
-- over the input of the transaction ref that created this output, concatenated with the lower bound
-- of the transaction interval (this must be bounded on both sides).
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
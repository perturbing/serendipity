{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE BinaryLiterals    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE NamedFieldPuns    #-}

module Validator where

import Plutus.VRF ( verifyVRF, Proof, Input (Input), Output (Output), PubKey (PubKey) )
import PlutusLedgerApi.V2.Contexts ( ScriptContext, TxId (..) )
import PlutusTx.Prelude
    ( Bool(..), (&&), BuiltinByteString, error, ($), (<>), (>=), Integer, (-), traceError, traceIfFalse )
import PlutusLedgerApi.Common ()
import PlutusTx ( BuiltinData, compile, CompiledCode )
import Utilities (writeCodeToFile, wrapValidator, i2osp)
import Prelude (IO)
import PlutusLedgerApi.V2
    ( ScriptContext(..), TxOutRef, ScriptPurpose(..), TxOutRef(..), TxInfo (..), Interval (..), LowerBound (..), UpperBound (..), Extended (..), POSIXTime (..) )
import PlutusTx.Builtins (blake2b_256)
import PlutusLedgerApi.V1 (POSIXTimeRange)


{-# INLINABLE  testValidator #-}
testValidator :: BuiltinByteString -> (Proof,Proof) -> ScriptContext -> Bool
testValidator _dtm (proof1,_proof2) ctx = checkSerendipity && checkCorrectOutput
    where
        -- Checks that the referenced VRF pkh with stake is allowed to unlock the output
        checkSerendipity :: Bool
        checkSerendipity = verifyVRF (Input ownRefBS) (Output (blake2b_256 (ownRefBS <> pkh))) (PubKey pkh) proof1 && txValidRangeDiff >= 1020000

        -- Checks that the reference VRF PKH hashes the Input correctly
        checkCorrectOutput :: Bool
        checkCorrectOutput = True

        txInfo :: PlutusLedgerApi.V2.TxInfo
        txInfo = scriptContextTxInfo ctx

        ownOutRef :: TxOutRef
        ownOutRef = getRef ctx
            where
                getRef :: ScriptContext -> TxOutRef
                getRef ScriptContext{scriptContextPurpose= Spending ref} = ref
                getRef _ = error ()

        ownRefBS = getTxId $ txOutRefId ownOutRef

        txValidRangeDiff :: Integer
        txValidRangeDiff = getDifference $ txInfoValidRange txInfo
            where 
                getDifference :: POSIXTimeRange -> Integer
                getDifference Interval{ivFrom=(LowerBound (Finite a) _), ivTo=(UpperBound (Finite b) _)} = getPOSIXTime $ b - a
                getDifference _ = traceError "Trace error: validity interval incorrect"

        pkh :: BuiltinByteString 
        pkh = i2osp 0x00


{-# INLINABLE  mkWrappedValidator #-}
mkWrappedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator = wrapValidator testValidator

validatorCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
validatorCode = $$(compile [|| mkWrappedValidator ||])

saveValidatorPolicy :: IO ()
saveValidatorPolicy = writeCodeToFile "../assets/test.plutus" validatorCode
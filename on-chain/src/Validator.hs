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
    ( Bool(..), (&&), BuiltinByteString, error, ($), (<>), (>=), Integer, (-), traceError, traceIfFalse
    , fst , (<), indexByteString, divide, (*))
import PlutusLedgerApi.Common ()
import PlutusTx ( BuiltinData, compile, CompiledCode, makeIsDataIndexed )
import Utilities (writeCodeToFile, wrapValidator, wrapPolicy, i2osp, os2ip)
import Prelude (IO)
import PlutusLedgerApi.V2
    ( ScriptContext(..), TxOutRef, ScriptPurpose(..), TxOutRef(..), TxInfo (..), Interval (..),
      LowerBound (..), UpperBound (..), Extended (..), POSIXTime (..), CurrencySymbol (..), unsafeFromBuiltinData )
import PlutusTx.Builtins (blake2b_256)
import PlutusLedgerApi.V1 (POSIXTimeRange)

data Redeemer = Redeemer {
    output :: Output,
    pubkey :: PubKey,
    proof  :: Proof
}
makeIsDataIndexed ''Redeemer [('Redeemer,0)]

{-# INLINABLE  testValidator #-}
testValidator :: CurrencySymbol -> BuiltinByteString -> Redeemer -> ScriptContext -> Bool
testValidator stakeSymbol dtm Redeemer{output,pubkey,proof} ctx = checkSerendipity && checkOutputThreshold output
    where
        checkSerendipity :: Bool
        checkSerendipity = verifyVRF (Input (ownRefBS <> dtm <> i2osp (fst txValidRangeInt))) output pubkey proof
        
        checkOutputThreshold :: Output -> Bool
        checkOutputThreshold (Output bs) = traceIfFalse "VRF to low" $ indexByteString bs 0 < 64

        txInfo :: PlutusLedgerApi.V2.TxInfo
        txInfo = scriptContextTxInfo ctx

        ownOutRef :: TxOutRef
        ownOutRef = getRef ctx
            where
                getRef :: ScriptContext -> TxOutRef
                getRef ScriptContext{scriptContextPurpose= Spending ref} = ref
                getRef _ = error ()

        ownRefBS = getTxId $ txOutRefId ownOutRef

        txValidRangeInt :: (Integer,Integer)
        txValidRangeInt = getDifference $ txInfoValidRange txInfo
            where 
                getDifference :: POSIXTimeRange -> (Integer,Integer)
                getDifference Interval{ivFrom=(LowerBound (Finite a) _), ivTo=(UpperBound (Finite b) _)} = (getPOSIXTime a, getPOSIXTime b)
                getDifference _ = traceError "Trace error: Validity interval incorrect"


{-# INLINABLE  mkWrappedValidator #-}
mkWrappedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator stakeSymbol = wrapValidator $ testValidator stakeSymbol'
    where
        stakeSymbol' = unsafeFromBuiltinData stakeSymbol

validatorCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
validatorCode = $$(compile [|| mkWrappedValidator ||])

saveValidator :: IO ()
saveValidator = writeCodeToFile "../assets/test.plutus" validatorCode

{-# INLINABLE freePolicy #-}
freePolicy :: () -> ScriptContext -> Bool
freePolicy _red _ctx = True

{-# INLINABLE mkWrappedFree #-}
mkWrappedFree :: BuiltinData -> BuiltinData -> ()
mkWrappedFree = wrapPolicy freePolicy

freePolicyCode :: CompiledCode (BuiltinData -> BuiltinData -> ())
freePolicyCode = $$(compile [|| mkWrappedFree ||])

saveFreePolicy :: IO ()
saveFreePolicy = writeCodeToFile "../assets/free-policy.plutus" freePolicyCode
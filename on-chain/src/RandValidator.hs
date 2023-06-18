{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use id" #-}

module RandValidator where

import Plutus.VRF                   ( verifyVRF, Proof, Input (Input), Output (Output), PubKey )
import PlutusLedgerApi.V2.Contexts  ( ScriptContext, TxId (..), findDatum )
import PlutusTx.Prelude             ( Bool(..), (&&), BuiltinByteString, error, ($), (<>), Integer
                                    , traceError, fst , (<), indexByteString, foldr, (==), (-)
                                    , head, maybe, otherwise, (.), Maybe (..), isJust, divide )
import PlutusTx                     ( BuiltinData, compile, CompiledCode, makeIsDataIndexed )
import Utilities                    ( writeCodeToFile, wrapValidator, i2osp )
import Prelude                      ( IO )
import PlutusLedgerApi.V2           ( ScriptContext(..), TxOutRef, ScriptPurpose(..), TxOutRef(..)
                                    , TxInfo (..), Interval (..), LowerBound (..), UpperBound (..)
                                    , Extended (..), POSIXTime (..), CurrencySymbol (..)
                                    , unsafeFromBuiltinData, TxInInfo (..), Address, TxOut (..)
                                    , Value (..), TokenName, OutputDatum (..), DatumHash (..), Datum (..), FromData (..) )
import PlutusLedgerApi.V1           ( POSIXTimeRange )
import PlutusTx.AssocMap            ( Map, empty, member, lookup, delete, elems )

{-# INLINABLE splitValue #-}
-- | `splitValue` is a utility function that takes a currency symbol and a value, 
-- | and returns a tuple of total value under the provided currency symbol and the residual value.
splitValue :: CurrencySymbol -> Value -> (Map TokenName Integer, Value)
splitValue symbol val
    | member symbol val'  = (maybe empty (\x -> x) (lookup symbol val'), Value (delete symbol val'))
    | otherwise           = (empty,val)
    where val' = getValue val

data DatumType = None | Hash | Inline 
makeIsDataIndexed ''DatumType [('None,0),('Hash,1),('Inline,2)]

-- The 'Datum' is the Address location where the newly create randommness should end up.
data MyDatum = MyDatum {
    address     :: Address,
    datumType   :: DatumType
}
makeIsDataIndexed ''MyDatum [('MyDatum,0)]

-- The 'Redeemer' is an ouput together with a proof that together is a valid VRF proof for the input
-- of the input reference concatinated with the transaction validity range.
data MyRedeemer = Redeemer {
    output :: Output,
    proof  :: Proof
}
makeIsDataIndexed ''MyRedeemer [('Redeemer,0)]

{-# INLINABLE  randValidator #-}
randValidator :: CurrencySymbol -> BuiltinByteString -> MyRedeemer -> ScriptContext -> Bool
randValidator stakeSymbol _addr Redeemer{output,proof} ctx = foldr (&&) checkSerendipity [checkOutputThreshold output, checkOutputLocation, checkValueConserved, checkTxValidRange]
    where
        -- This checks that the VRF proof + output in the redeemer, together with the pubkey from the referenced
        -- input and the input composed from concatinating the reference id and the lower bound of the transaction
        -- validity interval is valid.
        checkSerendipity :: Bool
        checkSerendipity = verifyVRF (Input (ownRefBS <> i2osp (divide (fst txValidRangeInt) 100))) output pubkey proof

        -- Check that the output of the VRF is lower than the treshold given by the stake of pubkey
        -- Note that a byte (intepreted as an integer) is strictly positive. So if stakeValue = 0
        -- The case that a validator has nothing at stake, this is always false.
        checkOutputThreshold :: Output -> Bool
        checkOutputThreshold (Output bs) = indexByteString bs 0 < stakeValue

        -- check here the address, but also that the datum is the hash of the output <> ownRefBS (the ouput is below a threshold)
        checkOutputLocation :: Bool
        checkOutputLocation = True

        checkValueConserved :: Bool
        checkValueConserved = True

        -- Check that the transaction validity interval is of size 100 seconds.
        checkTxValidRange :: Bool
        checkTxValidRange = (\(a,b)-> b - a) txValidRangeInt == 100

        -- Rest of the code are variable initializations and auxiliary function definitions to support the checks above.

        txInfo :: PlutusLedgerApi.V2.TxInfo
        txInfo = scriptContextTxInfo ctx

        -- 'ownOutRef' represents the transaction reference (Id + Index) of the output associated with the currently script being checked.
        ownOutRef :: TxOutRef
        ownOutRef = getRef ctx
            where
                getRef :: ScriptContext -> TxOutRef
                getRef ScriptContext{scriptContextPurpose= Spending ref} = ref
                getRef _ = error ()

        -- 'ownRefBS' is the transaction ID that create that output. This is used in the input of the VRF.
        ownRefBS :: BuiltinByteString
        ownRefBS = getTxId $ txOutRefId ownOutRef

        -- this is in seconds
        txValidRangeInt :: (Integer,Integer)
        txValidRangeInt = getDifference $ txInfoValidRange txInfo
            where
                getDifference :: POSIXTimeRange -> (Integer,Integer)
                getDifference Interval{ivFrom=(LowerBound (Finite a) _), ivTo=(UpperBound (Finite b) _)} = (divide (getPOSIXTime a) 1000, divide (getPOSIXTime b) 1000)
                getDifference _ = traceError "Trace error: Validity interval incorrect"

        -- `stakeRefUtxo` is the transaction output that contains the stake + pubkey used in the above VRF proof
        -- Note that this expects the reference to be the first input in the list. If this is not the case
        -- one cannot find/use the correct pubkey with its stake.
        stakeRefUtxo :: TxOut
        stakeRefUtxo = txInInfoResolved $ head (txInfoReferenceInputs txInfo)

        -- 'stakeValue' is the amount of staked tokens the reference input contains (if at all).
        -- Note that this expects the staked policy to have only one TokenName minted under its policy.
        stakeValue :: Integer
        stakeValue = head . elems . fst $ splitValue stakeSymbol (txOutValue stakeRefUtxo)

        stakeRefDatumHash :: Datum
        stakeRefDatumHash = case txOutDatum stakeRefUtxo of
          NoOutputDatum                 -> error ()
          OutputDatumHash (DatumHash h) -> if isJust maybeDatum
                                            then maybe (error ()) (\x -> x) maybeDatum
                                            else error ()
                                            where maybeDatum = findDatum (DatumHash h) txInfo
          OutputDatum _                 -> error ()

        pubkey :: PubKey
        pubkey = case fromBuiltinData (getDatum stakeRefDatumHash) of
            Just pk -> pk
            Nothing -> error ()

{-# INLINABLE  mkWrappedRandValidator #-}
mkWrappedRandValidator :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedRandValidator stakeSymbol = wrapValidator $ randValidator stakeSymbol'
    where
        stakeSymbol' = unsafeFromBuiltinData stakeSymbol

validatorRandCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
validatorRandCode = $$(compile [|| mkWrappedRandValidator ||])

saveRandValidator :: IO ()
saveRandValidator = writeCodeToFile "../assets/rand-validator.plutus" validatorRandCode
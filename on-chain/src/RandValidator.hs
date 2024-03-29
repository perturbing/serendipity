{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use id" #-}

module RandValidator where

import Plutus.VRF                   ( verifyVRF, Proof, Input (Input), Output (Output), PubKey )
import PlutusLedgerApi.V2.Contexts  ( ScriptContext, TxId (..), findDatum, txSignedBy )
import PlutusTx.Prelude             ( Bool(..), (&&), BuiltinByteString, error, ($), (<>), Integer
                                    , traceError, fst , (<), indexByteString, foldr, (==), (-)
                                    , head, maybe, otherwise, (.), Maybe (..), isJust, divide, find )
import PlutusTx                     ( BuiltinData, compile, CompiledCode, makeIsDataIndexed )
import Utilities                    ( writeCodeToFile, wrapValidator, i2osp )
import Prelude                      ( IO )
import PlutusLedgerApi.V2           ( ScriptContext(..), TxOutRef, ScriptPurpose(..), TxOutRef(..)
                                    , TxInfo (..), Interval (..), LowerBound (..), UpperBound (..)
                                    , Extended (..), POSIXTime (..), CurrencySymbol (..)
                                    , unsafeFromBuiltinData, TxInInfo (..), Address, TxOut (..)
                                    , Value (..), TokenName, OutputDatum (..), DatumHash (..)
                                    , Datum (..), FromData (..), PubKeyHash, adaSymbol, adaToken
                                    , singleton )
import PlutusLedgerApi.V1           ( POSIXTimeRange )
import PlutusTx.AssocMap            ( Map, empty, member, lookup, delete, elems )
import PlutusTx.Builtins            ( blake2b_256 )

-- [General notes on this module]
-- This module contains Plutus scripts responsible for governing the handling of oracle requests.
-- In the design of this application, a request is made by sending funds to the script address generated by this module.
-- The UTxO itself represents a request for randomness. The requester has the option to cancel it.
-- Alternatively, validators that have staked tokens locked in the stake validator can spend this UTxO. 
-- The conditions for this spending are that their VRF (Verifiable Random Function) value, 
-- derived from a combination of the UTxO ref being spent and some slot number, must be less than their stake.
-- Additionally, this script verifies that an output is created at a specified address by the requester and only a fee of 10 ada is taken.

-- Auxiliary function and data type definitions ------------------

{-# INLINABLE splitValue #-}
-- | `splitValue` is a helper function that accepts a currency symbol and a value, 
-- and yields a tuple consisting of the total value under the given currency symbol and the residual value.
-- If the symbol is not present in the value, it returns an empty Map and the value itself.
splitValue :: CurrencySymbol -> Value -> (Map TokenName Integer, Value)
splitValue symbol val
    | member symbol val'  = (maybe empty (\x -> x) (lookup symbol val'), Value (delete symbol val'))
    | otherwise           = (empty,val)
    where val' = getValue val

-- 'ReqDatum' represents the Address location where the newly created randomness should be sent.
-- It also captures the public key address of the requester for cancelation.
data ReqDatum = ReqDatum {
    requester   :: PubKeyHash,
    address     :: Address
}
makeIsDataIndexed ''ReqDatum [('ReqDatum,0)]

-- 'MintRandomness' carries an VRF output and a proof. Together, these form a valid VRF proof for the input
-- composed of the input reference concatenated with the transaction validity range.
data MintRandomness = MintRandomness {
    output :: Output,
    proof  :: Proof
}
makeIsDataIndexed ''MintRandomness [('MintRandomness,0)]

-- 'MyRedeemer' is either a Cancel or Mint action encapsulated within the MintRandomness data.
data MyRedeemer = Cancel | Mint MintRandomness
makeIsDataIndexed ''MyRedeemer [('Cancel,0),('Mint,1)]

-- | The main spending script of the randomness oracle. It is parametrizes over a currency symbol of the
-- staked tokens. This symbol should have a single associated asset class.
{-# INLINABLE  randValidator #-}
randValidator :: CurrencySymbol -> ReqDatum -> MyRedeemer -> ScriptContext -> Bool
randValidator stakeSymbol dtm red ctx = case red of
    Cancel                            -> txSignedBy txInfo (requester dtm)
    Mint MintRandomness{output,proof} -> foldr (&&) (checkSerendipity output proof) [checkOutputThreshold output, checkRequestOutput requestOutput output, checkValueConserved, checkTxValidRange]
    where

        -- 'checkSerendipity' ensures the VRF proof + output in the redeemer is valid. 
        -- It checks with the pubkey from the referenced input and the input composed 
        -- from concatenating the reference id and the lower bound of the transaction validity interval.
        checkSerendipity :: Output -> Proof ->  Bool
        checkSerendipity out prf = verifyVRF (Input (ownRefBS <> i2osp (divide (fst txValidRangeInt) 100))) out pubkey prf

        -- 'checkOutputThreshold' ensures the output of the VRF is lower than the threshold set by the stake of pubkey.
        checkOutputThreshold :: Output -> Bool
        checkOutputThreshold (Output bs) = indexByteString bs 0 < stakeValue

        -- 'checkRequestOutput' verifies that the datum of the created request output is the blake2b_256 hash of the output.
        -- The hash of the output is used since its first byte is below a threshold, and thus less entropic.
        checkRequestOutput :: TxOut -> Output -> Bool
        checkRequestOutput TxOut{txOutDatum} (Output bs) = case txOutDatum of
          NoOutputDatum -> False
          OutputDatumHash (DatumHash h) -> case findDatum (DatumHash h) txInfo of
                                            Nothing -> False
                                            Just (Datum bd) -> case fromBuiltinData bd of
                                                Nothing -> False
                                                Just bs' -> bs' == blake2b_256 bs
          OutputDatum _ -> False
            
        -- 'checkValueConserved' ensures the value of the created request output is 10 ada less than the input value.
        checkValueConserved :: Bool
        checkValueConserved = requestValueIn == txOutValue requestOutput <> singleton adaSymbol adaToken 10000000

        -- 'checkTxValidRange' verifies the transaction validity interval spans 100 seconds.
        checkTxValidRange :: Bool
        checkTxValidRange = (\(a,b)-> b - a) txValidRangeInt == 100

        -- Auxiliary variable initializations and function definitions to support the checks above.

        -- 'txInfo' provides detailed information about the transaction context.
        txInfo :: PlutusLedgerApi.V2.TxInfo
        txInfo = scriptContextTxInfo ctx

        -- 'ownOutRef' holds the transaction reference (Id + Index) of the output associated with the currently executed script.
        ownOutRef :: TxOutRef
        ownOutRef = getRef ctx
            where
                getRef :: ScriptContext -> TxOutRef
                getRef ScriptContext{scriptContextPurpose= Spending ref} = ref
                getRef _ = error ()

        -- 'ownRefBS' holds the transaction ID that created the output. It is used in the input of the VRF.
        ownRefBS :: BuiltinByteString
        ownRefBS = getTxId $ txOutRefId ownOutRef

        -- 'txValidRangeInt' represents the transaction's validity interval in seconds.
        txValidRangeInt :: (Integer,Integer)
        txValidRangeInt = getDifference $ txInfoValidRange txInfo
            where
                getDifference :: POSIXTimeRange -> (Integer,Integer)
                getDifference Interval{ivFrom=(LowerBound (Finite a) _), ivTo=(UpperBound (Finite b) _)} = (divide (getPOSIXTime a) 1000, divide (getPOSIXTime b) 1000)
                getDifference _ = traceError "Trace error: Validity interval incorrect"

        -- `stakeRefUtxo` refers to the transaction output that comprises the stake and the public key utilized in the preceding VRF proof.
        -- It assumes that the reference is the first input in the list. This condition is crucial because if it's not met,
        -- the script wouldn't be able to locate or utilize the correct public key along with its stake, which results in failure.
        stakeRefUtxo :: TxOut
        stakeRefUtxo = txInInfoResolved $ head (txInfoReferenceInputs txInfo)

        -- 'stakeValue' quantifies the volume of tokens staked in the reference input (if present).
        -- The script expects the staked policy to mint only one TokenName under its policy.
        stakeValue :: Integer
        stakeValue = head . elems . fst $ splitValue stakeSymbol (txOutValue stakeRefUtxo)

        -- 'stakeRefDatumHash' is the datum of the unspent transaction output (UTxO) that references the stake.
        stakeRefDatumHash :: Datum
        stakeRefDatumHash = case txOutDatum stakeRefUtxo of
          NoOutputDatum                 -> error ()
          OutputDatumHash (DatumHash h) -> if isJust maybeDatum
                                            then maybe (error ()) (\x -> x) maybeDatum
                                            else error ()
                                            where maybeDatum = findDatum (DatumHash h) txInfo
          OutputDatum _                 -> error ()

        -- 'pubkey' represents the VRF public key derived from the datum that is associated with the UTxO referencing the stake.
        pubkey :: PubKey
        pubkey = case fromBuiltinData (getDatum stakeRefDatumHash) of
            Just pk -> pk
            Nothing -> error ()

        -- 'requestOutput` is the output generated in this transaction for the purpose of propagating the request.
        requestOutput :: TxOut
        requestOutput = maybe (error ()) (\x -> x) (find reqOutFilter (txInfoOutputs txInfo))
            where
               reqOutFilter :: TxOut -> Bool
               reqOutFilter TxOut{txOutAddress} = txOutAddress == address dtm

        -- 'requestValueIn` corresponds to the total value at the request input that is being spent in the current transaction.
        requestValueIn :: Value
        requestValueIn = maybe (error ()) (txOutValue . txInInfoResolved) (find reqInFilter (txInfoInputs txInfo))
            where
                reqInFilter :: TxInInfo -> Bool
                reqInFilter TxInInfo{txInInfoOutRef} = txInInfoOutRef == ownOutRef

{-# INLINABLE  mkWrappedRandValidator #-}
mkWrappedRandValidator :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedRandValidator stakeSymbol = wrapValidator $ randValidator stakeSymbol'
    where
        stakeSymbol' = unsafeFromBuiltinData stakeSymbol

validatorRandCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
validatorRandCode = $$(compile [|| mkWrappedRandValidator ||])

saveRandValidator :: IO ()
saveRandValidator = writeCodeToFile "../assets/rand-validator.plutus" validatorRandCode
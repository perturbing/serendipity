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
                                    , Value (..), TokenName, OutputDatum (..), DatumHash (..), Datum (..), FromData (..), PubKeyHash, adaSymbol, adaToken, singleton )
import PlutusLedgerApi.V1           ( POSIXTimeRange )
import PlutusTx.AssocMap            ( Map, empty, member, lookup, delete, elems )
import PlutusTx.Builtins            ( blake2b_256 )

-- [General notes on this file]
-- This file contains the plutus scripts that governs when and how oracle requests is handled.
-- In the design of this application, a requester sends some funds to the below script address.
-- This UTxO on its own is a request for randomness, and the requester has the option to cancel it.
-- Another option is that validators that have stake locked in the stake validator, can spend this utxo. 
-- The conditions for this spending are that their VRF value, given by some combination of the UTxO ref 
-- that is being spend and some slot number, is lower than their stake (which the reference in the transaction).
-- Besides that, this script also checks that an output is created at a specified address by the requester.
-- The datum of this contains two things

--------------------- helper functions and types -------------

{-# INLINABLE splitValue #-}
-- | `splitValue` is a utility function that takes a currency symbol and a value, 
-- | and returns a tuple of total value under the provided currency symbol and the residual value.
splitValue :: CurrencySymbol -> Value -> (Map TokenName Integer, Value)
splitValue symbol val
    | member symbol val'  = (maybe empty (\x -> x) (lookup symbol val'), Value (delete symbol val'))
    | otherwise           = (empty,val)
    where val' = getValue val

-- The 'Datum' is the Address location where the newly create randommness should end up.
data ReqDatum = ReqDatum {
    requester   :: PubKeyHash,
    address     :: Address
}
makeIsDataIndexed ''ReqDatum [('ReqDatum,0)]

-- The 'Redeemer' is an ouput together with a proof that together is a valid VRF proof for the input
-- of the input reference concatinated with the transaction validity range.
data MintRandomness = MintRandomness {
    output :: Output,
    proof  :: Proof
}
makeIsDataIndexed ''MintRandomness [('MintRandomness,0)]

data MyRedeemer = Cancel | Mint MintRandomness
makeIsDataIndexed ''MyRedeemer [('Cancel,0),('Mint,1)]

-- | The core spending script of the randomness oracle.
-- | This script is parametrised by the currency symbol of
-- | the stake tokens. This symbol thus should only have one asset class.
{-# INLINABLE  randValidator #-}
randValidator :: CurrencySymbol -> ReqDatum -> MyRedeemer -> ScriptContext -> Bool
randValidator stakeSymbol dtm red ctx = case red of
    Cancel                            -> txSignedBy txInfo (requester dtm)
    Mint MintRandomness{output,proof} -> foldr (&&) (checkSerendipity output proof) [checkOutputThreshold output, checkRequestOutput requestOutput output, checkValueConserved, checkTxValidRange]
    where
        -- This checks that the VRF proof + output in the redeemer, together with the pubkey from the referenced
        -- input and the input composed from concatinating the reference id and the lower bound of the transaction
        -- validity interval is valid.
        checkSerendipity :: Output -> Proof ->  Bool
        checkSerendipity out prf = verifyVRF (Input (ownRefBS <> i2osp (divide (fst txValidRangeInt) 100))) out pubkey prf

        -- Check that the output of the VRF is lower than the treshold given by the stake of pubkey
        -- Note that a byte (intepreted as an integer) is strictly positive. So if stakeValue = 0,
        -- the case that a validator has nothing at stake, this is always false.
        checkOutputThreshold :: Output -> Bool
        checkOutputThreshold (Output bs) = indexByteString bs 0 < stakeValue

        -- Check here that the datum of the created request output is the hash of the output (the ouput is below a threshold so not entropic enough)
        checkRequestOutput :: TxOut -> Output -> Bool
        checkRequestOutput TxOut{txOutDatum} (Output bs) = case txOutDatum of
          NoOutputDatum -> False
          OutputDatumHash (DatumHash h) -> case findDatum (DatumHash h) txInfo of
                                            Nothing -> False
                                            Just (Datum bd) -> case fromBuiltinData bd of
                                                Nothing -> False
                                                Just bs' -> bs' == blake2b_256 bs
          OutputDatum _ -> False
            
        -- Check here that the value of the create request output is 10 ada less than the input value.
        checkValueConserved :: Bool
        checkValueConserved = requestValueIn == txOutValue requestOutput <> singleton adaSymbol adaToken 10000000

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

        -- 'stakeRefDatumHash' is the datum of the utxo that holds the stake that is being referenced.
        stakeRefDatumHash :: Datum
        stakeRefDatumHash = case txOutDatum stakeRefUtxo of
          NoOutputDatum                 -> error ()
          OutputDatumHash (DatumHash h) -> if isJust maybeDatum
                                            then maybe (error ()) (\x -> x) maybeDatum
                                            else error ()
                                            where maybeDatum = findDatum (DatumHash h) txInfo
          OutputDatum _                 -> error ()

        -- 'pubkey' is the VRF public key extracted from the datum that is utxo that holds the stake that is being referenced.
        pubkey :: PubKey
        pubkey = case fromBuiltinData (getDatum stakeRefDatumHash) of
            Just pk -> pk
            Nothing -> error ()

        -- 'requestOutput` is the output that is creating in this transaction that propagates the request.
        requestOutput :: TxOut
        requestOutput = maybe (error ()) (\x -> x) (find reqOutFilter (txInfoOutputs txInfo))
            where
               reqOutFilter :: TxOut -> Bool
               reqOutFilter TxOut{txOutAddress} = txOutAddress == address dtm

        -- 'requestValueIn` is the total value at the request input that is currently being spend
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
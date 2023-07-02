{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Testing where

import PlutusTx.Prelude         ( Integer, Bool (..), ($), (==), (&&), any, traceIfFalse, negate )
import PlutusLedgerApi.V1.Value ( flattenValue )
import PlutusTx                 ( BuiltinData, compile, CompiledCode, makeIsDataIndexed )
import Utilities                ( writeCodeToFile, wrapPolicy, wrapValidator )
import Prelude                  ( IO )
import PlutusLedgerApi.V2       ( ScriptContext(..), TxOutRef(..), TokenName(..), TxInfo(..)
                                , TxOutRef, TxId(..), txInInfoOutRef,  unsafeFromBuiltinData )

-- 'freePolicy' is a minting policy that always returns true. The policy's logic is obfuscated
-- to serve as a "private" testing policy for statistical analysis on-chain.
{-# INLINABLE freePolicy #-}
freePolicy :: () -> ScriptContext -> Bool
freePolicy _red _ctx = a == b
  where 
    a :: Integer
    a = 2

    b :: Integer
    b = 2

{-# INLINABLE mkWrappedFree #-}
mkWrappedFree :: BuiltinData -> BuiltinData -> ()
mkWrappedFree = wrapPolicy freePolicy

freePolicyCode :: CompiledCode (BuiltinData -> BuiltinData -> ())
freePolicyCode = $$(compile [|| mkWrappedFree ||])

saveFreePolicy :: IO ()
saveFreePolicy = writeCodeToFile "../assets/free-policy.plutus" freePolicyCode

-- 'freeVal' is a spending validator that always returns true. The validator's logic is obfuscated
-- to serve as a "private" testing validator for statistical analysis on-chain.
{-# INLINABLE freeVal #-} -- a little easter egg: https://www.youtube.com/watch?v=1lWJXDG2i0A 
freeVal :: () -> () -> ScriptContext -> Bool
freeVal _dtm _red _ctx = a == b
  where 
    a :: Integer
    a = 2

    b :: Integer
    b = 2

{-# INLINABLE mkWrappedFreeVal #-}
mkWrappedFreeVal :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedFreeVal = wrapValidator freeVal

freeValCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
freeValCode = $$(compile [|| mkWrappedFreeVal ||])

saveFreeValidator :: IO ()
saveFreeValidator = writeCodeToFile "../assets/free-validator.plutus" freeValCode

-- The 'Redeemer' data type represents two possible actions - 'Mint' or 'Burn'
data Redeemer = Mint | Burn
makeIsDataIndexed ''Redeemer [('Mint,0),('Burn,1)]

-- 'mkStakePolicy' is a one-shot fungible token minting policy. It is used to mint a specific number of tokens once.
-- Additionally, the policy enforces the minting of precisely 255 tokens.
-- It has two variants of a 'Redeemer': 'Mint' and 'Burn'. 
-- If 'Mint' is used, it checks if the correct UTxO was consumed and whether the correct amount was minted.
-- If 'Burn' is used, it verifies that the right amount has been burned.
{-# INLINABLE mkStakePolicy #-}
mkStakePolicy :: TxOutRef -> TokenName -> Redeemer -> ScriptContext -> Bool
mkStakePolicy oref tn red ctx = case red of
    Mint -> traceIfFalse "UTxO not consumed"   hasUTxO        &&
            traceIfFalse "wrong amount minted" (checkMintedAmount 255)
    Burn -> traceIfFalse "wrong amount burned" $ checkMintedAmount (negate 255)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    checkMintedAmount :: Integer -> Bool
    checkMintedAmount n = case flattenValue (txInfoMint info) of
        [(_, tn'', amt)] -> tn'' == tn && amt == n
        _                -> False

{-# INLINABLE mkWrappedStakePolicy #-}
mkWrappedStakePolicy :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedStakePolicy tid ix tn' = wrapPolicy $ mkStakePolicy oref tn
  where
    oref :: TxOutRef
    oref = TxOutRef
        (TxId $ unsafeFromBuiltinData tid)
        (unsafeFromBuiltinData ix)

    tn :: TokenName
    tn = unsafeFromBuiltinData tn'

stakePolicyCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
stakePolicyCode = $$(PlutusTx.compile [|| mkWrappedStakePolicy ||])

saveStakeCode :: IO ()
saveStakeCode = writeCodeToFile "../assets/stake-nft-policy.plutus" stakePolicyCode
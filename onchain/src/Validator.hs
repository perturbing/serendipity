{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE NamedFieldPuns    #-}

module Validator where

import PlutusLedgerApi.V2.Contexts ( ScriptContext )
import PlutusTx.Prelude ( Bool(True))
import PlutusLedgerApi.Common ()
import PlutusTx
import Utilities (wrapPolicy,writeCodeToFile)
import Prelude (IO)


{-# INLINABLE  mkFree #-}
mkFree :: () -> ScriptContext -> Bool
mkFree _red _ctx = True

{-# INLINABLE  mkWrappedFree #-}
mkWrappedFree :: BuiltinData -> BuiltinData -> ()
mkWrappedFree = wrapPolicy mkFree

freeCode :: CompiledCode (BuiltinData -> BuiltinData -> ())
freeCode = $$(compile [|| mkWrappedFree ||])

saveFreePolicy :: IO ()
saveFreePolicy = writeCodeToFile "../assets/alwaysTrue-policy.plutus" freeCode
{-# LANGUAGE LambdaCase #-}

module Utilities.Serialize where

import           Cardano.Api           (Error (displayError), PlutusScript,
                                        PlutusScriptV2, writeFileTextEnvelope)
import           Cardano.Api.Shelley   (PlutusScript (..), File (..))
import           Codec.Serialise       (Serialise, serialise)
import qualified Data.ByteString.Lazy  as BSL
import qualified Data.ByteString.Short as BSS
import qualified PlutusLedgerApi.V2    as PlutusV2
import           PlutusTx              (CompiledCode)

-- can be replaced with LedgerAPI.V2 serialiseCompiledCode
serializableToScript :: Serialise a => a -> PlutusScript PlutusScriptV2
serializableToScript = PlutusScriptSerialised . BSS.toShort . BSL.toStrict . serialise

-- Serialize compiled code
codeToScript :: CompiledCode a -> PlutusScript PlutusScriptV2
codeToScript = serializableToScript . PlutusV2.serialiseCompiledCode

-- Create file with Plutus script
writeScriptToFile :: FilePath -> PlutusScript PlutusScriptV2 -> IO ()
writeScriptToFile filePath script =
  writeFileTextEnvelope (File filePath) Nothing script >>= \case
    Left err -> print $ displayError err
    Right () -> putStrLn $ "Serialized script to: " ++ filePath

-- Create file with compiled code
writeCodeToFile :: FilePath -> CompiledCode a -> IO ()
writeCodeToFile filePath = writeScriptToFile filePath . codeToScript
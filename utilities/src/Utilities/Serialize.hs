{-# LANGUAGE LambdaCase #-}

module Utilities.Serialize where

import           Cardano.Api           (Error (displayError), PlutusScript,
                                        PlutusScriptV2, writeFileTextEnvelope)
import           Cardano.Api.Shelley   (PlutusScript (..), File (..))
import qualified PlutusLedgerApi.V2    as PlutusV2
import           PlutusTx              (CompiledCode)

-- Create file with Plutus script
writeScriptToFile :: FilePath -> PlutusScript PlutusScriptV2 -> IO ()
writeScriptToFile filePath script =
  writeFileTextEnvelope (File filePath) Nothing script >>= \case
    Left err -> print $ displayError err
    Right () -> putStrLn $ "Serialized script to: " ++ filePath

-- Create file with compiled code
writeCodeToFile :: FilePath -> CompiledCode a -> IO ()
writeCodeToFile filePath = writeScriptToFile filePath . PlutusScriptSerialised . PlutusV2.serialiseCompiledCode
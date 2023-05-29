{-# LANGUAGE NoImplicitPrelude #-}

module Utilities.Misc where

import           PlutusTx.Builtins
import           PlutusTx.Prelude

i2osp :: Integer -> BuiltinByteString
i2osp n
    | n < 0     = error ()
    | n == 0    = consByteString 0 emptyByteString
    | otherwise = go n
    where go m
            | m == 0    = emptyByteString
            | otherwise = go (m `quotient` 256) <> consByteString (m `remainder` 256) emptyByteString
{-# INLINABLE i2osp #-}
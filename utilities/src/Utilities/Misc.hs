{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Utilities.Misc where

import           PlutusTx.Builtins
import           PlutusTx.Prelude

{-# INLINABLE i2osp #-}
i2osp :: Integer -> BuiltinByteString
i2osp n
    | n < 0     = traceError "i2osp fail"
    | n == 0    = consByteString 0 emptyByteString
    | otherwise = go n
    where go m
            | m == 0    = emptyByteString
            | otherwise = go (m `quotient` 256) <> consByteString (m `remainder` 256) emptyByteString

{-# INLINABLE os2ip #-}
os2ip :: BuiltinByteString -> Integer
os2ip bs
    | bs == emptyByteString = traceError "os2ip fail"
    | otherwise             = go bs
    where len xs = lengthOfByteString xs - 1
          intAtLastByte xs = indexByteString xs $ len xs
          stripLastByte xs = takeByteString (len xs) xs
          go xs
            | xs == emptyByteString = 0
            | otherwise             = intAtLastByte xs + 256 * go (stripLastByte xs)
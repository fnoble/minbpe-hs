-- |
-- Module      : MinBPE.Codec.Alternate
-- Copyright   : (c) 2024 Fergus Noble
--
-- License     : MIT
-- Maintainer  : fergusnoble@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Alternative 'MinBPE.Codec' implementations for performance comparison and
-- development.
--
-- In general these should not be used as the best general purpose
-- implementation is exported from 'MinBPE.Codec'.
module MinBPE.Codec.Alternate
  ( decode1,
    decode2,
    decodeVec1,
  )
where

import Data.ByteString qualified as B
import Data.ByteString.Builder qualified as Build
import Data.ByteString.Lazy qualified as L
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import MinBPE.Types

-- | Naive implementation of decode.
-- This implementation simply maps the 'Token's to their corresponding
-- 'B.ByteString's using 'Map' lookup and then concatenates them together.
decode1 :: Vocab -> [Token] -> T.Text
decode1 vocab ts = TE.decodeUtf8Lenient $ B.concat $ map (vocab Map.!) ts

-- | Implementation of decode using 'Data.ByteString.Builder'.
-- This implementation uses 'Data.ByteString.Builder' to lazily assemble the
-- decoded bytes with fewer allocations. This implementation still uses 'Map'
-- lookup to find 'Token' mappings.
decode2 :: Vocab -> [Token] -> T.Text
decode2 vocab ts =
  TE.decodeUtf8Lenient $
    L.toStrict $
      Build.toLazyByteString $
        mconcat $
          map (\t -> Build.byteString (vocab Map.! t)) ts

-- | Implementation of decode using 'Data.ByteString.Builder' and 'Data.Vector'.
-- This implementation uses 'Data.ByteString.Builder' to lazily assemble the
-- decoded bytes with fewer allocations. This implementation uses a vocab stored
-- as a 'VocabVector' to improve lookup performance.
decodeVec1 :: VocabVector -> [Token] -> T.Text
decodeVec1 vocab ts =
  TE.decodeUtf8Lenient $
    L.toStrict $
      Build.toLazyByteString $
        mconcat $
          map (\t -> Build.byteString (vocab V.! t)) ts

-- |
-- Module      : MinBPE.Types
-- Copyright   : (c) 2024 Fergus Noble
--
-- License     : MIT
-- Maintainer  : fergusnoble@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Core 'MinBPE' types
module MinBPE.Types
  ( Token,
    Pair,
    Merge,
    MergeList,
    Vocab,
    VocabVector,
  )
where

import Data.ByteString qualified as B
import Data.Map (Map)
import Data.Vector qualified as V

-- | Base type for a 'Token'
type Token = Int

-- | A pair of 'Token's
type Pair = (Token, Token)

-- | An individual BPE merge
type Merge = (Pair, Token)

-- | A list of 'Merge's that defines a specific BPE
type MergeList = [Merge]

-- | A 'Vocab' maps from 'Token's to their decoded 'B.ByteString's
-- A vocab is used by the decoder and can be obtained from a 'MergeList'
-- or loaded directly e.g. from a TikToken file.
--
-- 'Vocab' uses 'Map' internally and has the advantage of being easy to
-- manipulate or extend, but can be slower to access.
type Vocab = Map Token B.ByteString

-- | 'VocabVector' is a 'V.Vector' representation of a 'Vocab' that is used by
-- some decoder implementations for improved performance.
--
-- 'VocabVector' uses 'V.Vector' internally and has the advantage of having fast
-- O(1) lookup, but is generally harder to manipulate and slower to extend.
--
-- 'VocabVector' also has the limitation that the 'Token's must be numbered
-- sequentially from 0 to @NUM_TOKENS - 1@.
type VocabVector = V.Vector B.ByteString

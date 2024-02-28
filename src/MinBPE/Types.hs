module MinBPE.Types
  ( Token,
    Pair,
    Merge,
    MergeList,
    Vocab,
  )
where

import Data.ByteString qualified as B
import Data.Map (Map)

-- | Base type for a 'Token'
type Token = Int

-- | A pair of 'Token's
type Pair = (Token, Token)

-- | An individual BPE merge
type Merge = (Pair, Token)

-- | A list of 'Merges' that defines a specific BPE
type MergeList = [Merge]

-- | A 'Vocab' maps from 'Tokens' to their decoded 'ByteString's
-- A vocab is used by the decoder and can be obtained from a 'MergeList'
-- or loaded directly e.g. from a TikToken file.
type Vocab = Map Token B.ByteString

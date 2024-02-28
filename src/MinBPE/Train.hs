module MinBPE.Train
  ( train,
  )
where

import Data.ByteString qualified as B
import Data.ByteString.Builder
import Data.ByteString.Lazy qualified as L
import Data.ByteString.Short qualified as B
  ( ShortByteString,
    toShort,
  )
import Data.List (maximumBy)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Ord (comparing)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Data.Word ()
import MinBPE.Internal
import MinBPE.Types

type Pairs = Map Pair Int

pairs' :: Pairs -> [Token] -> Pairs
pairs' ps [] = ps
pairs' ps [_] = ps
pairs' ps (t1 : t2 : ts) = pairs' (Map.insertWith (+) (t1, t2) 1 ps) (t2 : ts)

-- | Extract a 'Map' from all the unique 'Token' pairs in the input to their number of occurances
pairs :: [Token] -> Pairs
pairs = pairs' Map.empty

-- | Find the 'Pair' with the greatest number of occurances in a 'Token' sequence
topPair :: [Token] -> Pair
topPair ts = fst $ maximumBy (comparing (\((_, _), v) -> v)) $ Map.toList $ pairs ts

train' :: [Token] -> [Merge] -> Token -> Int -> ([Merge], [Token])
train' ts ms _ 0 = (ms, ts)
train' ts ms t n = train' (merge p t ts) ((p, t) : ms) (t + 1) (n - 1)
  where
    p = topPair ts

-- | Create 'Merge' list from training data by applying successive rounds of BPE
train ::
  -- | Training data as 'Token' sequence
  [Token] ->
  -- | Number of rounds of BPE merging to apply
  Int ->
  -- | 'Merge' list
  [Merge]
train ts n = reverse $ fst $ train' ts [] (utf8TokenMax + 1) n
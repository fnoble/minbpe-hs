module MinBPE.Internal
  ( utf8TokenMax,
    toRawTokens,
    merge,
  )
where

import Data.ByteString qualified as B
import MinBPE.Types

utf8TokenMax :: (Integral a) => a
utf8TokenMax = 255

-- | Converts a 'B.ByteString' to a list of 'Token' without applying BPE
-- 'Token' values are simply the raw byte values (e.g. UTF-8 encoded)
toRawTokens :: B.ByteString -> [Token]
toRawTokens = map fromIntegral . B.unpack

-- | Replace all occurances of 'Pair' with 'Token' in the 'Token' sequence
merge ::
  -- | 'Pair' to match
  Pair ->
  -- | 'Token' to replace with
  Token ->
  -- | Input 'Token' sequence
  [Token] ->
  [Token]
merge p@(p1, p2) tp (t1 : t2 : ts)
  | (p1 == t1) && (p2 == t2) = tp : merge p tp ts
  | otherwise = t1 : merge p tp (t2 : ts)
merge _ _ ts = ts

module MinBPE.Codec
  ( toRawTokens,
    pairs,
    topPair,
    merge,
    train,
    makeVocab,
    makeVocabVector,
    vocabToVector,
    shortVocabVec,
    checkVocabSequential,
    decode,
    decodeBS,
    decodeBS2,
    decodeBS3,
    decodeBS4,
    encode,
    encodeBS,
  )
where

import Data.ByteString qualified as B
import Data.ByteString.Builder
import Data.ByteString.Lazy qualified as L
import Data.ByteString.Short qualified as B
  ( ShortByteString,
    fromShort,
    toShort,
  )
import Data.List (maximumBy)
import Data.Map (Map, (!))
import Data.Map qualified as Map
import Data.Ord (comparing)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Data.Word ()
import MinBPE.Types

type Pairs = Map Pair Int

-- | Converts a 'ByteString' to a list of 'Token' without applying BPE
toRawTokens :: B.ByteString -> [Token]
toRawTokens = map fromIntegral . B.unpack

pairs' :: Pairs -> [Token] -> Pairs
pairs' ps [] = ps
pairs' ps [t] = ps
pairs' ps (t1 : t2 : ts) = pairs' (Map.insertWith (+) (t1, t2) 1 ps) (t2 : ts)

-- | Extract a 'Map' from all the unique 'Token' pairs in the input to their number of occurances
pairs :: [Token] -> Pairs
pairs = pairs' Map.empty

-- | Find the 'Pair' with the greatest number of occurances in a 'Token' sequence
topPair :: [Token] -> Pair
topPair ts = fst $ maximumBy (comparing (\((t1, t2), v) -> v)) $ Map.toList $ pairs ts

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
merge p tp ts = ts

train' :: [Token] -> [Merge] -> Token -> Int -> ([Merge], [Token])
train' ts ms _ 0 = (ms, ts)
train' ts ms t n = train' (merge p t ts) ((p, t) : ms) (t + 1) (n - 1)
  where
    p = topPair ts

utf8TokenMax :: (Integral a) => a
utf8TokenMax = 255

-- | Create 'Merge' list from training data by applying successive rounds of BPE
train ::
  -- | Training data as 'Token' sequence
  [Token] ->
  -- | Number of rounds of BPE merging to apply
  Int ->
  -- | 'Merge' list
  [Merge]
train ts n = reverse $ fst $ train' ts [] (utf8TokenMax + 1) n

baseUTF8Vocab :: Vocab
baseUTF8Vocab = Map.fromList $ zipWith (\t c -> (t, B.pack [c])) [0 .. utf8TokenMax] [0 .. utf8TokenMax]

makeVocab' :: [Merge] -> Vocab -> Vocab
makeVocab' [] vocab = vocab
makeVocab' (((p1, p2), t) : ms) vocab = makeVocab' ms $ Map.insert t bs vocab
  where
    bs = B.append (vocab ! p1) (vocab ! p2)

makeVocab :: [Merge] -> Vocab
makeVocab ms = makeVocab' ms baseUTF8Vocab

checkVocabSequential :: Vocab -> Bool
checkVocabSequential v = idxs == [0 .. l]
  where
    idxs = map fst $ Map.toAscList v
    l = length idxs

vocabToVector :: Vocab -> V.Vector B.ByteString
-- TODO: Check tokens really are sequential and equal to Vector indices
vocabToVector v = V.fromList $ map snd $ Map.toAscList v

makeVocabVector :: [Merge] -> V.Vector B.ByteString
makeVocabVector = vocabToVector . makeVocab

decode :: Vocab -> [Token] -> T.Text
decode vocab ts = TE.decodeUtf8Lenient $ B.concat $ map (vocab !) ts

decodeBS :: Vocab -> [Token] -> B.ByteString
decodeBS vocab ts = TE.encodeUtf8 $ decode vocab ts

-- | Token sequences may not always decode to valid UTF-8.
-- Replaces any invalid bytes with the Unicode replacement character U+FFFD
fixUTF8 :: B.ByteString -> B.ByteString
fixUTF8 = TE.encodeUtf8 . TE.decodeUtf8Lenient

decodeBS2 :: Vocab -> [Token] -> B.ByteString
decodeBS2 vocab ts = fixUTF8 $ L.toStrict $ toLazyByteString $ mconcat $ map (\t -> byteString (vocab ! t)) ts

decodeBS3 :: V.Vector B.ByteString -> [Token] -> B.ByteString
decodeBS3 vocab ts = fixUTF8 $ L.toStrict $ toLazyByteString $ mconcat $ map (\t -> byteString (vocab V.! t)) ts

shortVocabVec = V.map B.toShort

decodeBS4 :: V.Vector B.ShortByteString -> [Token] -> B.ByteString
decodeBS4 vocab ts = fixUTF8 $ L.toStrict $ toLazyByteString $ mconcat $ map (\t -> shortByteString (vocab V.! t)) ts

encode' :: [Merge] -> [Token] -> [Token]
encode' [] ts = ts
encode' ((p, t) : ms) ts = encode' ms $ merge p t ts

encodeBS :: [Merge] -> B.ByteString -> [Token]
encodeBS ms bs = encode' ms $ toRawTokens bs

encode :: [Merge] -> T.Text -> [Token]
encode ms t = encodeBS ms (TE.encodeUtf8 t)
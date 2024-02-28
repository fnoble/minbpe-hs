module MinBPE.Codec
  ( -- * Vocab
    -- $vocab
    makeVocab,
    vocabToVector,

    -- * Decoding
    decode,
    decodeBS,
    decodeVec,
    decodeVecBS,

    -- * Encoding
    encode,
    encodeBS,
  )
where

import Data.ByteString qualified as B
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import MinBPE.Codec.Alternate qualified as Alt
import MinBPE.Internal
import MinBPE.Types
import MinBPE.Types (MergeList)

-- $vocab
-- A vocab (e.g. 'Vocab' or 'VocabVector') stores a mapping from 'Token's to
-- their corresponding decoded bytes. The deocders in this library use 'Vocab'
-- instead of the 'MergeList' directly to improve performance, and also as some
-- token encodings (e.g.  TikToken files) are natively supplied in this format.
--
-- This module contains several functions for creating and converting vocabs.

-- | The base vocab with only the UTF-8 bytes (0..255) included, i.e. no merges
baseUTF8Vocab :: Vocab
baseUTF8Vocab = Map.fromList $ zipWith (\t c -> (t, B.pack [c])) [0 .. utf8TokenMax] [0 .. utf8TokenMax]

makeVocab' :: MergeList -> Vocab -> Vocab
makeVocab' [] vocab = vocab
makeVocab' (((p1, p2), t) : ms) vocab = makeVocab' ms $ Map.insert t bs vocab
  where
    bs = B.append (vocab Map.! p1) (vocab Map.! p2)

-- | Create a 'Vocab' from a 'MergeList'
makeVocab :: MergeList -> Vocab
makeVocab ms = makeVocab' ms baseUTF8Vocab

checkVocabSequential :: Vocab -> Bool
checkVocabSequential v = idxs == [0 .. l]
  where
    idxs = map fst $ Map.toAscList v
    l = length idxs

-- | Convert a 'Vocab' to a 'VocabVector' representation.
vocabToVector :: Vocab -> VocabVector
-- TODO: Check tokens really are sequential and equal to Vector indices
vocabToVector v = V.fromList $ map snd $ Map.toAscList v

decode :: Vocab -> [Token] -> T.Text
decode = Alt.decode1

decodeBS :: Vocab -> [Token] -> B.ByteString
decodeBS vocab ts = TE.encodeUtf8 $ decode vocab ts

decodeVec :: VocabVector -> [Token] -> T.Text
decodeVec = Alt.decodeVec1

decodeVecBS :: VocabVector -> [Token] -> B.ByteString
decodeVecBS vocab ts = TE.encodeUtf8 $ decodeVec vocab ts

encode' :: [Merge] -> [Token] -> [Token]
encode' [] ts = ts
encode' ((p, t) : ms) ts = encode' ms $ merge p t ts

encodeBS :: [Merge] -> B.ByteString -> [Token]
encodeBS ms bs = encode' ms $ toRawTokens bs

encode :: [Merge] -> T.Text -> [Token]
encode ms t = encodeBS ms (TE.encodeUtf8 t)
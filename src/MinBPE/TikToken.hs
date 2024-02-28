-- |
-- Module      : MinBPE.TikToken
-- Copyright   : (c) 2024 Fergus Noble
--
-- License     : MIT
-- Maintainer  : fergusnoble@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Utilities for loading an saving 'Vocab's from TikToken files
module MinBPE.TikToken
  ( loadTikToken,
  )
where

import Data.Attoparsec.ByteString.Char8 qualified as Atto
import Data.ByteString qualified as B
import Data.ByteString.Base64 (decodeBase64)
import Data.Map qualified as Map
import MinBPE.Types

-- | Matches an base64 encoded string and decodes it to a raw 'B.ByteString'
base64 :: Atto.Parser B.ByteString
base64 = do
  raw <- Atto.takeWhile1 (\c -> Atto.isAlpha_ascii c || Atto.isDigit c || Atto.inClass "+/=" c)
  case decodeBase64 raw of
    Left err -> fail $ "Error decoding Base64: " ++ show err
    Right bs -> return bs

-- | Parses a single line of a TikToken file
parseEntry :: Atto.Parser (Token, B.ByteString)
parseEntry = do
  bs <- base64
  _ <- Atto.space
  t <- Atto.decimal
  return (t, bs)

toVocab :: [(Token, B.ByteString)] -> Vocab
toVocab = Map.fromList

-- | Parses an entire TikToken file
parseFile :: Atto.Parser Vocab
parseFile = fmap toVocab $ Atto.many1 $ parseEntry <* Atto.endOfLine

-- | Load a 'Vocab' from a TikToken file
loadTikToken :: FilePath -> IO Vocab
loadTikToken f = do
  raw <- B.readFile f
  case Atto.parseOnly parseFile raw of
    Left err -> fail $ "Error loading TikToken file: " ++ err
    Right v -> return v

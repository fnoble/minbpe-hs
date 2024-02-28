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
  ( -- * Loading
    loadTikToken,

    -- * Writing
    buildTikToken,
    writeTikToken,
  )
where

import Data.Attoparsec.ByteString.Char8 qualified as Atto
import Data.ByteString qualified as B
import Data.ByteString.Base64 (decodeBase64, encodeBase64')
import Data.ByteString.Builder qualified as Build
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

-- | Parses an entire TikToken file
parseFile :: Atto.Parser Vocab
parseFile = fmap Map.fromList $ Atto.many1 $ parseEntry <* Atto.endOfLine

-- | Load a 'Vocab' from a TikToken file
loadTikToken :: FilePath -> IO Vocab
loadTikToken f = do
  raw <- B.readFile f
  case Atto.parseOnly parseFile raw of
    Left err -> fail $ "Error loading TikToken file: " ++ err
    Right v -> return v

-- | Build a single entry in a TikToken file.
-- In the TikToken format, each line consists of the decoded byte sequence
-- represented as a Base64 encoded string, followed by the corresponding token
-- identifier as a decimal.
buildEntry :: (Token, B.ByteString) -> Build.Builder
buildEntry (t, bs) = Build.byteString bs64 <> Build.charUtf8 ' ' <> Build.intDec t
  where
    bs64 = encodeBase64' bs

-- | Build an entire TikToken file.
buildTikToken :: Vocab -> Build.Builder
buildTikToken v = mconcat [buildEntry e <> Build.charUtf8 '\n' | e <- Map.toAscList v]

-- | Write a 'Vocab' to a TikToken file
writeTikToken :: FilePath -> Vocab -> IO ()
writeTikToken f v = Build.writeFile f (buildTikToken v)

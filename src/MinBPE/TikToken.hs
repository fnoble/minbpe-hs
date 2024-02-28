module MinBPE.TikToken
  ( loadTikToken,
  )
where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8 qualified as Atto
import Data.ByteString qualified as B
import Data.ByteString.Base64 (decodeBase64)
import Data.Map (Map, (!))
import Data.Map qualified as Map
import MinBPE.Types

base64 :: Atto.Parser B.ByteString
base64 = do
  raw <- Atto.takeWhile1 (\c -> Atto.isAlpha_ascii c || Atto.isDigit c || Atto.inClass "+/=" c)
  case decodeBase64 raw of
    Left err -> fail $ "Error decoding Base64: " ++ show err
    Right bs -> return bs

parseEntry :: Atto.Parser (Token, B.ByteString)
parseEntry = do
  bs <- base64
  Atto.space
  t <- Atto.decimal
  return (t, bs)

toVocab :: [(Token, B.ByteString)] -> Vocab
toVocab = Map.fromList

parseFile :: Atto.Parser Vocab
parseFile = fmap toVocab $ Atto.many1 $ parseEntry <* Atto.endOfLine

loadTikToken :: FilePath -> IO Vocab
loadTikToken f = do
  raw <- B.readFile f
  case Atto.parseOnly parseFile raw of
    Left err -> fail $ "Error loading TikToken file: " ++ err
    Right v -> return v

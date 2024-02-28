module Main (main) where

import Criterion.Main
import Data.ByteString qualified as B
import MinBPE.Codec

main :: IO ()
main = do
  inputBSSwift <- B.readFile "./example/taylorswift.txt"
  inputBSShakespeare <- B.readFile "./example/shakespeare.txt"
  print $ B.length inputBSSwift
  print $ B.length inputBSShakespeare
  let tokens = toRawTokens inputBSSwift
  let ms = train tokens 20
  let vocab = makeVocab ms
  let vocabVec = makeVocabVector ms
  let vocabVecShort = shortVocabVec vocabVec
  let encodedSwift = encodeBS ms inputBSSwift
  let encodedShakespeare = encodeBS ms inputBSShakespeare
  defaultMain
    [ bgroup
        "training"
        [ bench "20" $ nf (train tokens) 20
        ],
      bgroup
        "encode"
        [ bench "taylorswift" $ nf (encodeBS ms) inputBSSwift,
          bench "shakespeare" $ nf (encodeBS ms) inputBSShakespeare
        ],
      bgroup
        "decode"
        [ bench "taylorswift" $ nf (decodeBS vocab) encodedSwift,
          bench "shakespeare" $ nf (decodeBS vocab) encodedShakespeare,
          bench "shakespeare2" $ nf (decodeBS2 vocab) encodedShakespeare,
          bench "shakespeare3" $ nf (decodeBS3 vocabVec) encodedShakespeare,
          bench "shakespeare4" $ nf (decodeBS4 vocabVecShort) encodedShakespeare
        ]
    ]

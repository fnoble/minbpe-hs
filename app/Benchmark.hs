module Main (main) where

import Criterion.Main
import Data.ByteString qualified as B
import MinBPE.Codec
import MinBPE.Codec.Alternate qualified as Alt
import MinBPE.Internal
import MinBPE.Train

main :: IO ()
main = do
  inputBSSwift <- B.readFile "./example/taylorswift.txt"
  inputBSShakespeare <- B.readFile "./example/shakespeare.txt"
  print $ B.length inputBSSwift
  print $ B.length inputBSShakespeare
  let tokens = toRawTokens inputBSSwift
  let ms = train tokens 20
  let vocab = makeVocab ms
  let vocabVec = vocabToVector vocab
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
        [ bench "taylorswift" $ nf (decode vocab) encodedSwift,
          bench "shakespeare" $ nf (decode vocab) encodedShakespeare,
          bench "shakespeare_vec" $ nf (decodeVec vocabVec) encodedShakespeare,
          bench "shakespeare_alt2" $ nf (Alt.decode2 vocab) encodedShakespeare
        ]
    ]

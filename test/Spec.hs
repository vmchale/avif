module Main (main) where

import           Codec.Avif
import           Control.DeepSeq  (deepseq)
import qualified Data.ByteString  as BS
import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = defaultMain $
    testGroup "Roundtrip"
        [ decodeNoThrow "test/data/original.avif"
        , decEncNoThrow "test/data/original.avif"
        ]

decodeNoThrow :: FilePath -> TestTree
decodeNoThrow fp = testCase fp $ do
    res <- decode <$> BS.readFile fp
    assertBool "Doesn't throw exception" (res `deepseq` True)

decEncNoThrow :: FilePath -> TestTree
decEncNoThrow fp = testCase fp $ do
    bytes <- BS.readFile fp
    let res = encode (decode bytes)
    res @?= bytes

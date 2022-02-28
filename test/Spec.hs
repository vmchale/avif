module Main (main) where

import           Codec.Avif
import qualified Data.ByteString  as BS
import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = defaultMain $
    testGroup "Roundtrip"
        [ decodeNoThrow "test/data/original.avif"
        ]

decodeNoThrow :: FilePath -> TestTree
decodeNoThrow fp = testCase fp $ do
    res <- decode =<< BS.readFile fp
    assertBool "Doesn't throw exception" (res `seq` True)

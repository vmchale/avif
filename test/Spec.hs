{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Codec.Avif
import           Control.DeepSeq  (deepseq)
import qualified Data.ByteString  as BS
import           Data.Either      (isLeft)
import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = defaultMain $
    testGroup "Roundtrip"
        [ decodeNoThrow "test/data/original.avif"
        , decEncNoThrow "test/data/original.avif"
        , decodeFail
        ]

decodeFail :: TestTree
decodeFail = testCase "decodeE" $
    let res = decodeE "aaaaa"
    in assertBool "fails on bad input" $ isLeft res

decodeNoThrow :: FilePath -> TestTree
decodeNoThrow fp = testCase fp $ do
    res <- decode <$> BS.readFile fp
    assertBool "Doesn't throw exception" (res `deepseq` True)

decEncNoThrow :: FilePath -> TestTree
decEncNoThrow fp = testCase fp $ do
    bytes <- BS.readFile fp
    let res = encode (decode bytes)
    assertBool "Doesn't throw exception" (res `deepseq` True)

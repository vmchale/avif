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
        -- , decodeRGB16 "test/data/woman.avif"
        , decodeFail
        ]

decodeFail :: TestTree
decodeFail = testCase "decodeE" $
    let res = decodeE "aaaaa"
    in assertBool "fails on bad input" $ isLeft res

decodeRGB16 :: FilePath -> TestTree
decodeRGB16 fp = testCase fp $ do
    res <- decodeE <$> BS.readFile fp
    case res of
        Right ImageRGBA16{} -> assertBool "decodes 16-bit" True
        Right _             -> assertFailure "expected 16-bit color."
        Left err            -> assertFailure (show err)

decodeNoThrow :: FilePath -> TestTree
decodeNoThrow fp = testCase fp $ do
    res <- decodeE <$> BS.readFile fp
    case res of
        Left err -> assertFailure (show err)
        Right{}  -> assertBool "Doesn't error" True

decEncNoThrow :: FilePath -> TestTree
decEncNoThrow fp = testCase fp $ do
    bytes <- BS.readFile fp
    let res = encode ((\(ImageRGBA8 img) -> img) $ decode bytes)
    assertBool "Doesn't throw exception" (res `deepseq` True)

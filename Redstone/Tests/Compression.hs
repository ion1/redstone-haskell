{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Redstone.Tests.Compression (tests) where

import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.TH
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Control.Applicative
import qualified Control.Exception as Ex
import qualified Data.ByteString as BS
import Data.List
import Data.Word

import Redstone.Compression
import Redstone.Error

instance Arbitrary CompressionType where
  arbitrary = elements [minBound .. maxBound]
  shrink x | x /= minBound = [minBound]
           | otherwise     = []

tests :: Test
tests = $(testGroupGenerator)

prop_compressDecompressAuto :: CompressionType -> [Word8] -> Property
prop_compressDecompressAuto cType (BS.pack -> dataIn) =
  monadicIO $
    do dataOut <- run (decompress Nothing =<< compress cType dataIn)
       assert (dataIn == dataOut)

prop_compressDecompressSame :: CompressionType -> [Word8] -> Property
prop_compressDecompressSame cType (BS.pack -> dataIn) =
  monadicIO $
    do dataOut <- run (decompress (Just cType) =<< compress cType dataIn)
       assert (dataIn == dataOut)

prop_compressDecompressWrong :: CompressionType -> [Word8] -> Property
prop_compressDecompressWrong cTypeCom (BS.pack -> dataIn) =
  monadicIO $
    do cTypeDec <- pick . elements $ delete cTypeCom [minBound .. maxBound]
       pre (cTypeCom /= cTypeDec)
       assert =<< run (act cTypeDec)
  where
    act cTypeDec =
      Ex.handle (\(_ :: RedstoneError) -> return True)
        $ do _ <- decompress (Just cTypeDec) =<< compress cTypeCom dataIn
             return False

prop_compressGetType :: CompressionType -> [Word8] -> Property
prop_compressGetType cTypeIn (BS.pack -> dataIn) =
  monadicIO $
    do cTypeOut <- run (getCompressionType <$> compress cTypeIn dataIn)
       assert (Just cTypeIn == cTypeOut)

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
    do Right dataOut <- run (comprDecompr cType Nothing dataIn)
       assert (dataIn == dataOut)

prop_compressDecompressSame :: CompressionType -> [Word8] -> Property
prop_compressDecompressSame cType (BS.pack -> dataIn) =
  monadicIO $
    do Right dataOut <- run (comprDecompr cType (Just cType) dataIn)
       assert (dataIn == dataOut)

prop_compressDecompressWrong :: CompressionType -> [Word8] -> Property
prop_compressDecompressWrong cTypeCom (BS.pack -> dataIn) =
  monadicIO $
    do cTypeDec <- pick . elements $ delete cTypeCom [minBound .. maxBound]
       pre (cTypeCom /= cTypeDec)  -- Redundant, but let’s make sure.
       Left _err <- run (comprDecompr cTypeCom (Just cTypeDec) dataIn)
       return ()

comprDecompr :: CompressionType -> Maybe CompressionType -> BS.ByteString
             -> IO (Either RedstoneError BS.ByteString)
comprDecompr cTypeCom cTypeDec dataIn =
  Ex.try (decompress cTypeDec =<< compress cTypeCom dataIn)

prop_compressGetType :: CompressionType -> [Word8] -> Property
prop_compressGetType cTypeIn (BS.pack -> dataIn) =
  monadicIO $
    do cTypeOut <- run (getCompressionType <$> compress cTypeIn dataIn)
       assert (Just cTypeIn == cTypeOut)

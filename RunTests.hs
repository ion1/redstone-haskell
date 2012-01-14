module Main (main) where

import Test.Framework (defaultMain)

import qualified Redstone.Tests.Compression as Compression

main :: IO ()
main = defaultMain [ Compression.tests ]

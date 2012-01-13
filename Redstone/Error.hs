{-# LANGUAGE DeriveDataTypeable #-}

module Redstone.Error
( RedstoneError (..)
) where

import Control.Exception
import Data.Typeable

data RedstoneError = RedstoneError String
  deriving (Show, Typeable)
instance Exception RedstoneError

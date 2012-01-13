{-# LANGUAGE ScopedTypeVariables #-}

module Redstone.Compression
( CompressionType (..)
, decompress
, compress
, getCompressionType
) where

import Control.Exception
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import Foreign
import Foreign.C

import Redstone.Error
import Redstone.Prim.Compression
import Redstone.Prim.Memory

data CompressionType = Gzip | Zlib
  deriving (Eq, Ord, Bounded, Enum, Read, Show)

type CompressionFunction = C'RSCompressionType -> Ptr Word8 -> CSize
                         -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()

-- FIXME: When libredstone stops printing error messages, these should be
-- referentially transparent.

decompress :: Maybe CompressionType -> BS.ByteString -> IO BS.ByteString
decompress = apply "decompress" c'rs_decompress

compress :: CompressionType -> BS.ByteString -> IO BS.ByteString
compress = apply "compress" c'rs_compress . Just

apply :: String -> CompressionFunction -> Maybe CompressionType
      -> BS.ByteString -> IO BS.ByteString
apply name func cType dataIn =
  BS.unsafeUseAsCStringLen dataIn $ \(c'dataIn, lenIn) ->
  alloca $ \(c'dataOutPtr :: Ptr (Ptr Word8)) ->
  alloca $ \(c'lenOutPtr  :: Ptr CSize) ->
    do func (compressionTypeToC cType)
            (castPtrCCW8 c'dataIn) (fromIntegral lenIn)
            c'dataOutPtr c'lenOutPtr
       c'dataOut <- peek c'dataOutPtr
       c'lenOut  <- peek c'lenOutPtr
       when (c'dataOut == nullPtr)
            (throwIO (RedstoneError (name ++ " failed")))
       BS.unsafePackCStringFinalizer c'dataOut (fromIntegral c'lenOut)
                                     (c'rs_free c'dataOut)

getCompressionType :: BS.ByteString -> Maybe CompressionType
getCompressionType dataIn =
  unsafeLocalState $
  BS.unsafeUseAsCStringLen dataIn $ \(c'dataIn, lenIn) ->
    let c'type = c'rs_get_compression_type (castPtrCCW8 c'dataIn)
                                           (fromIntegral lenIn)
    in  return (compressionTypeFromC c'type)

castPtrCCW8 :: Ptr CChar -> Ptr Word8
castPtrCCW8 = castPtr

compressionTypeToC :: Maybe CompressionType -> C'RSCompressionType
compressionTypeToC Nothing     = c'RS_AUTO_COMPRESSION
compressionTypeToC (Just Gzip) = c'RS_GZIP
compressionTypeToC (Just Zlib) = c'RS_ZLIB

compressionTypeFromC :: C'RSCompressionType -> Maybe CompressionType
compressionTypeFromC t
  | t == c'RS_AUTO_COMPRESSION    = Nothing
  | t == c'RS_GZIP                = Just Gzip
  | t == c'RS_ZLIB                = Just Zlib
  | t == c'RS_UNKNOWN_COMPRESSION = Nothing
  | otherwise = error ("Invalid compression type " ++ show t)

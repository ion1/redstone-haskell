{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

#include <bindings.dsl.h>

#include <libredstone/compression.h>

module Redstone.Prim.Compression where
#strict_import

#integral_t RSCompressionType
#num RS_AUTO_COMPRESSION
#num RS_GZIP
#num RS_ZLIB
#num RS_UNKNOWN_COMPRESSION

#ccall rs_decompress ,  <RSCompressionType> -> Ptr Word8 -> CSize \
                     -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()

#ccall rs_compress ,  <RSCompressionType> -> Ptr Word8 -> CSize \
                   -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()

#ccall rs_get_compression_type , Ptr Word8 -> CSize -> <RSCompressionType>

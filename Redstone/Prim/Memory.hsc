{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

#include <bindings.dsl.h>

#include <libredstone/compression.h>

module Redstone.Prim.Memory where
#strict_import

#ccall rs_free , Ptr a -> IO ()

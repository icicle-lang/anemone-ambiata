{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Anemone.Foreign.Mempool (
    Mempool(..)
  , create
  , alloc
  , allocBytes
  , calloc
  , callocBytes
  , free
  , totalAllocSize
  , isPointerAligned
  ) where

import           Anemone.Foreign.Data ( CBool(..) )

import           Foreign.C.Types ( CInt (..), CSize(..) )
import           Foreign.Ptr ( Ptr )
import           Foreign.Storable ( Storable(..) )

import           P
import           Data.Void (Void)

import           System.IO (IO)

import qualified Prelude as Savage


-- | The Mempool constructor must be exposed to work in FFI calls.
-- However, it must not be used in any other places.
-- The only way to construct a Mempool is by calling @create@.
--
-- This infinite knot here should enforce this.
newtype Mempool =
  Mempool (Ptr Void)

foreign import ccall unsafe "anemone_mempool_create"
  create :: IO Mempool

foreign import ccall unsafe "hs_anemone_mempool_alloc"
  allocBytes :: Mempool -> CSize -> IO (Ptr a)

foreign import ccall unsafe "hs_anemone_mempool_calloc"
  callocBytes :: Mempool -> CSize -> CSize -> IO (Ptr a)

foreign import ccall unsafe "anemone_mempool_free"
  free :: Mempool -> IO ()

foreign import ccall unsafe "anemone_mempool_total_alloc_size"
  totalAllocSize :: Mempool -> IO Int64

foreign import ccall unsafe "anemone_is_pointer_aligned"
  isPointerAligned :: Ptr Void -> IO CBool

alloc :: forall a. Storable a => Mempool -> IO (Ptr a)
alloc mp =
  allocBytes mp (fromIntegral $ sizeOf (Savage.undefined :: a))

calloc :: forall a. Storable a => Mempool -> CSize -> IO (Ptr a)
calloc mp i =
  callocBytes mp i (fromIntegral $ sizeOf (Savage.undefined :: a))

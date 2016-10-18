{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Anemone.Foreign.Mempool (
    Mempool
  , create
  , alloc
  , allocBytes
  , free
  ) where

import           Anemone.Foreign.Data

import           Foreign.C.Types ( CSize(..) )
import           Foreign.Ptr
import           Foreign.Storable ( Storable(..) )

import           P

import           System.IO (IO)

import qualified Prelude as Savage


newtype Mempool =
  Mempool (Ptr ())

foreign import ccall unsafe "anemone_mempool_create"
  create :: IO Mempool

foreign import ccall unsafe "hs_anemone_mempool_alloc"
  allocBytes :: Mempool -> CSize -> IO (Ptr a)

foreign import ccall unsafe "anemone_mempool_free"
  free :: Mempool -> IO ()

alloc :: forall a. Storable a => Mempool -> IO (Ptr a)
alloc mp =
  allocBytes mp (fromIntegral $ sizeOf (Savage.undefined :: a))


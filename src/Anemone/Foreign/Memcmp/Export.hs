{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Anemone.Foreign.Memcmp.Export (
    memcmp
  , memcmp8
  , memcmp64
  , memcmp128_unsafe
  , memcmp_partial_load64
  ) where

import           Anemone.Foreign.Data
import           Anemone.Foreign.Memcmp.Base

import qualified Data.ByteString as B

import           P

-- | Normal, blessed memory comparison
memcmp :: B.ByteString -> B.ByteString -> Ordering
memcmp = wrapCmp hs_anemone_memcmp

foreign import ccall unsafe
    hs_anemone_memcmp
    :: MemcmpT_Raw

-------------------------------------
-- Zoo of memory comparison functions
-------------------------------------

memcmp8 :: B.ByteString -> B.ByteString -> Ordering
memcmp8 = wrapCmp hs_anemone_memcmp8

memcmp64 :: B.ByteString -> B.ByteString -> Ordering
memcmp64 = wrapCmp hs_anemone_memcmp64

memcmp128_unsafe :: B.ByteString -> B.ByteString -> Ordering
memcmp128_unsafe = wrapCmp hs_anemone_memcmp128_unsafe

memcmp_partial_load64 :: B.ByteString -> B.ByteString -> Ordering
memcmp_partial_load64 = wrapCmp hs_anemone_memcmp_partial_load64

foreign import ccall unsafe
    hs_anemone_memcmp8
    :: MemcmpT_Raw

foreign import ccall unsafe
    hs_anemone_memcmp64
    :: MemcmpT_Raw

foreign import ccall unsafe
    hs_anemone_memcmp_partial_load64
    :: MemcmpT_Raw

foreign import ccall unsafe
    hs_anemone_memcmp128_unsafe
    :: MemcmpT_Raw

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Anemone.Foreign.Memcmp.Bench (
    memcmp8_bench
  , memcmp64_bench
  , memcmp128_bench
  , memcmp_bench
  
  , memeq8_bench
  , memeq64_bench
  , memeq128_bench
  , memeq_bench
  ) where

import Anemone.Foreign.Memcmp.Base


memcmp8_bench       :: MemcmpT
memcmp8_bench       = wrapCmp anemone_memcmp8_bench

memcmp64_bench      :: MemcmpT
memcmp64_bench      = wrapCmp anemone_memcmp64_bench

memcmp128_bench     :: MemcmpT
memcmp128_bench     = wrapCmp anemone_memcmp128_bench

memcmp_bench        :: MemcmpT
memcmp_bench        = wrapCmp anemone_memcmp_bench


memeq8_bench        :: MemeqT
memeq8_bench        = wrapEq anemone_memeq8_bench

memeq64_bench       :: MemeqT
memeq64_bench       = wrapEq anemone_memeq64_bench

memeq128_bench      :: MemeqT
memeq128_bench      = wrapEq anemone_memeq128_bench

memeq_bench         :: MemeqT
memeq_bench         = wrapEq anemone_memeq_bench


foreign import ccall unsafe
    anemone_memcmp8_bench
    :: MemcmpT_Raw

foreign import ccall unsafe
    anemone_memcmp64_bench
    :: MemcmpT_Raw

foreign import ccall unsafe
    anemone_memcmp128_bench
    :: MemcmpT_Raw

foreign import ccall unsafe
    anemone_memcmp_bench
    :: MemcmpT_Raw


foreign import ccall unsafe
    anemone_memeq8_bench
    :: MemcmpT_Raw

foreign import ccall unsafe
    anemone_memeq64_bench
    :: MemcmpT_Raw

foreign import ccall unsafe
    anemone_memeq128_bench
    :: MemcmpT_Raw

foreign import ccall unsafe
    anemone_memeq_bench
    :: MemcmpT_Raw


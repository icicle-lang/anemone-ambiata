{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Anemone.Foreign.Memcmp.Export (
    memcmp8
  , memcmp64
  , memcmp128
  , memcmp
  
  , memeq8
  , memeq64
  , memeq128
  , memeq
  ) where

import Anemone.Foreign.Memcmp.Base


memcmp8       :: MemcmpT
memcmp8       = wrapCmp anemone_memcmp8

memcmp64      :: MemcmpT
memcmp64      = wrapCmp anemone_memcmp64

memcmp128     :: MemcmpT
memcmp128     = wrapCmp anemone_memcmp128

memcmp        :: MemcmpT
memcmp        = wrapCmp anemone_memcmp


memeq8        :: MemeqT
memeq8        = wrapEq anemone_memeq8

memeq64       :: MemeqT
memeq64       = wrapEq anemone_memeq64

memeq128      :: MemeqT
memeq128      = wrapEq anemone_memeq128

memeq         :: MemeqT
memeq         = wrapEq anemone_memeq


foreign import ccall unsafe
    anemone_memcmp8
    :: MemcmpT_Raw

foreign import ccall unsafe
    anemone_memcmp64
    :: MemcmpT_Raw

foreign import ccall unsafe
    anemone_memcmp128
    :: MemcmpT_Raw

foreign import ccall unsafe
    anemone_memcmp
    :: MemcmpT_Raw


foreign import ccall unsafe
    anemone_memeq8
    :: MemcmpT_Raw

foreign import ccall unsafe
    anemone_memeq64
    :: MemcmpT_Raw

foreign import ccall unsafe
    anemone_memeq128
    :: MemcmpT_Raw

foreign import ccall unsafe
    anemone_memeq
    :: MemcmpT_Raw



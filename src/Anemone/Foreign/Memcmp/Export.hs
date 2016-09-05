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

import           Anemone.Foreign.Data
import           Anemone.Foreign.Memcmp.Base


memcmp8       :: MemcmpT
memcmp8       = wrapCmp hs_anemone_memcmp8

memcmp64      :: MemcmpT
memcmp64      = wrapCmp hs_anemone_memcmp64

memcmp128     :: MemcmpT
memcmp128     = wrapCmp hs_anemone_memcmp128

memcmp        :: MemcmpT
memcmp        = wrapCmp hs_anemone_memcmp


memeq8        :: MemeqT
memeq8        = wrapEq hs_anemone_memeq8

memeq64       :: MemeqT
memeq64       = wrapEq hs_anemone_memeq64

memeq128      :: MemeqT
memeq128      = wrapEq hs_anemone_memeq128

memeq         :: MemeqT
memeq         = wrapEq hs_anemone_memeq


foreign import ccall unsafe
    hs_anemone_memcmp8
    :: MemcmpT_Raw

foreign import ccall unsafe
    hs_anemone_memcmp64
    :: MemcmpT_Raw

foreign import ccall unsafe
    hs_anemone_memcmp128
    :: MemcmpT_Raw

foreign import ccall unsafe
    hs_anemone_memcmp
    :: MemcmpT_Raw


foreign import ccall unsafe
    hs_anemone_memeq8
    :: MemcmpT_Raw

foreign import ccall unsafe
    hs_anemone_memeq64
    :: MemcmpT_Raw

foreign import ccall unsafe
    hs_anemone_memeq128
    :: MemcmpT_Raw

foreign import ccall unsafe
    hs_anemone_memeq
    :: MemcmpT_Raw



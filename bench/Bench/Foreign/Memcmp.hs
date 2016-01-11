{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Bench.Foreign.Memcmp (
    memcmp8_bench
  , memcmp64_bench
  , memcmp128_bench
  , memcmp_bench
  , memcmp_std_bench
  
  , memeq8_bench
  , memeq64_bench
  , memeq128_bench
  , memeq_bench
  ) where

import P

type MembenchT = Int -> Int

foreign import ccall unsafe
    "anemone_memcmp8_bench"
    memcmp8_bench
    :: MembenchT

foreign import ccall unsafe
    "anemone_memcmp64_bench"
    memcmp64_bench
    :: MembenchT

foreign import ccall unsafe
    "anemone_memcmp128_bench"
    memcmp128_bench
    :: MembenchT

foreign import ccall unsafe
    "anemone_memcmp_bench"
    memcmp_bench
    :: MembenchT

foreign import ccall unsafe
    "anemone_std_memcmp_bench"
    memcmp_std_bench
    :: MembenchT



foreign import ccall unsafe
    "anemone_memeq8_bench"
    memeq8_bench
    :: MembenchT

foreign import ccall unsafe
    "anemone_memeq64_bench"
    memeq64_bench
    :: MembenchT

foreign import ccall unsafe
    "anemone_memeq128_bench"
    memeq128_bench
    :: MembenchT

foreign import ccall unsafe
    "anemone_memeq_bench"
    memeq_bench
    :: MembenchT



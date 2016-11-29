{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Bench.Foreign.Memcmp (
    memcmp8_simple_bench
  , memcmp64_simple_bench
  , memcmp128_unsafe_simple_bench
  , memcmp_partial_load64_simple_bench
  , memcmp_simple_bench
  , memcmp_std_simple_bench

  , memcmp8_regs_bench
  , memcmp64_regs_bench
  , memcmp128_unsafe_regs_bench
  , memcmp_partial_load64_regs_bench
  , memcmp_regs_bench
  , memcmp_std_regs_bench
  ) where

import P

type MembenchT = Int -> Int

foreign import ccall unsafe
    "anemone_memcmp8_simple_bench"
    memcmp8_simple_bench
    :: MembenchT

foreign import ccall unsafe
    "anemone_memcmp64_simple_bench"
    memcmp64_simple_bench
    :: MembenchT

foreign import ccall unsafe
    "anemone_memcmp128_unsafe_simple_bench"
    memcmp128_unsafe_simple_bench
    :: MembenchT

foreign import ccall unsafe
    "anemone_memcmp_partial_load64_simple_bench"
    memcmp_partial_load64_simple_bench
    :: MembenchT


foreign import ccall unsafe
    "anemone_memcmp_simple_bench"
    memcmp_simple_bench
    :: MembenchT

foreign import ccall unsafe
    "anemone_std_memcmp_simple_bench"
    memcmp_std_simple_bench
    :: MembenchT

foreign import ccall unsafe
    "anemone_memcmp8_regs_bench"
    memcmp8_regs_bench
    :: MembenchT

foreign import ccall unsafe
    "anemone_memcmp64_regs_bench"
    memcmp64_regs_bench
    :: MembenchT

foreign import ccall unsafe
    "anemone_memcmp128_unsafe_regs_bench"
    memcmp128_unsafe_regs_bench
    :: MembenchT

foreign import ccall unsafe
    "anemone_memcmp_partial_load64_regs_bench"
    memcmp_partial_load64_regs_bench
    :: MembenchT


foreign import ccall unsafe
    "anemone_memcmp_regs_bench"
    memcmp_regs_bench
    :: MembenchT

foreign import ccall unsafe
    "anemone_std_memcmp_regs_bench"
    memcmp_std_regs_bench
    :: MembenchT



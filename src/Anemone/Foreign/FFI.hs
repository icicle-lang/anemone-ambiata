{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Anemone.Foreign.FFI (
    ffi_id_safe
  , ffi_id_unsafe
  ) where

import P

foreign import ccall
    "anemone_ffi_id"
    ffi_id_safe
    :: Int
    -> Int

foreign import ccall unsafe
    "anemone_ffi_id"
    ffi_id_unsafe
    :: Int
    -> Int


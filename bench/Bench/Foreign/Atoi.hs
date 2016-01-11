{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Bench.Foreign.Atoi (
    string_to_i64_bench
  , string_to_i64_v128_bench
  , string_to_i64_std_bench
  ) where

import P

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B

import Foreign.C.String
import System.IO.Unsafe


string_to_i64_bench :: AtoiBenchT
string_to_i64_bench
 = wrapBench anemone_string_to_i64_bench

string_to_i64_v128_bench :: AtoiBenchT
string_to_i64_v128_bench
 = wrapBench anemone_string_to_i64_v128_bench

string_to_i64_std_bench :: AtoiBenchT
string_to_i64_std_bench
 = wrapBench anemone_std_atoi_bench


foreign import ccall unsafe
    anemone_string_to_i64_bench
    :: AtoiBenchT_Raw

foreign import ccall unsafe
    anemone_string_to_i64_v128_bench
    :: AtoiBenchT_Raw

foreign import ccall unsafe
    anemone_std_atoi_bench
    :: AtoiBenchT_Raw




type AtoiBenchT     = B.ByteString -> Int
type AtoiBenchT_Raw = CString -> Int -> Int

wrapBench :: AtoiBenchT_Raw -> AtoiBenchT
wrapBench f a
 = unsafePerformIO
 $ B.unsafeUseAsCString a
 $ \a'
 -> return $ f a' (B.length a)


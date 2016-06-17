{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Bench.Foreign.Strtod (
    strtod_bench
  , strtod_std_bench
  , strtod_hs_read_double_bench
  , strtod_hs_read_rational_bench
  ) where

import P

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B

import qualified Data.Text.Encoding as TextE
import qualified Data.Text.Read as TextR

import Foreign.C.String
import System.IO.Unsafe


strtod_bench :: StrtodBenchT
strtod_bench
 = wrapBench anemone_strtod_bench

strtod_std_bench :: StrtodBenchT
strtod_std_bench
 = wrapBench anemone_std_strtod_bench

strtod_hs_read_double_bench :: StrtodBenchT
strtod_hs_read_double_bench bs
 = loop (TextE.decodeUtf8 bs)
 where
  loop
   = let go 0  !i _ = i
         go !n !i !t
          = case TextR.double t of
             Left  !_ -> go (n-1) i t
             Right !_ -> go (n-1) (i+1) t
     in  go (1000000 :: Int) (0 :: Int)

strtod_hs_read_rational_bench :: StrtodBenchT
strtod_hs_read_rational_bench bs
 = loop (TextE.decodeUtf8 bs)
 where
  loop
   = let go 0  !i _ = i
         go !n !i !t
          = case rat t of
             Left  !_ -> go (n-1) i t
             Right !_ -> go (n-1) (i+1) t
     in  go (1000000 :: Int) (0 :: Int)

  rat :: TextR.Reader Double
  rat = TextR.rational


foreign import ccall unsafe
    anemone_strtod_bench
    :: StrtodBenchT_Raw

foreign import ccall unsafe
    anemone_std_strtod_bench
    :: StrtodBenchT_Raw




type StrtodBenchT     = B.ByteString -> Int
type StrtodBenchT_Raw = CString -> Int -> Int

wrapBench :: StrtodBenchT_Raw -> StrtodBenchT
wrapBench f a
 = unsafePerformIO
 -- Make sure this is null terminated!
 -- Only strtod needs null termination
 $ B.unsafeUseAsCString (a <> "\0")
 $ \a'
 -> return $ f a' (B.length a)


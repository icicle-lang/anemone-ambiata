{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wwarn #-}

import qualified Anemone.Foreign.FFI as FoFFI
import qualified Bench.Foreign.Memcmp as FoMemcmp
import qualified Bench.Foreign.Atoi as FoAtoi

import           Criterion.Main
import           Criterion.Types (Config(..))

import           System.IO (IO)
import           Data.ByteString.Char8 ()

import           P



main :: IO ()
main
 = defaultMainWith benchConfig
 [ bench_ffi
 , bench_memcmp
 , bench_atoi
 ]

benchConfig :: Config
benchConfig =
  defaultConfig {
      reportFile = Just "dist/build/anemone-bench.html"
    , csvFile    = Just "dist/build/anemone-bench.csv"
    }


bench_ffi :: Benchmark
bench_ffi
 = bgroup "Foreign function interface"
 [ bench "None (return unit)"       $ nfIO $ loop $ (return ())
 , bench "Pure (id 0)"              $ nfIO $ loop $ (return $ id (0 :: Int))
 , bench "Safe (ffi_id_safe)"       $ nfIO $ loop $ (return $ FoFFI.ffi_id_safe   0)
 , bench "Unsafe (ffi_id_unsafe)"   $ nfIO $ loop $ (return $ FoFFI.ffi_id_unsafe 0)
 ]

 where
  loop f
   = let go 0 = return ()
         go n = f >>= \a -> seq a (go (n - 1))
     in  go (100000 :: Int)


bench_memcmp :: Benchmark
bench_memcmp
 = bgroup "Memory comparisons"
 [ bgroup "Ordered compare"
     [ go_all "memcmp8"             FoMemcmp.memcmp8_bench
     , go_all "memcmp64"            FoMemcmp.memcmp64_bench
     , go_all "memcmp128"           FoMemcmp.memcmp128_bench
     , go_all "memcmp std"          FoMemcmp.memcmp_std_bench
     ]
 , bgroup "Equality compare"
     [ go_all "memeq8"                  FoMemcmp.memeq8_bench
     , go_all "memeq64"                 FoMemcmp.memeq64_bench
     , go_all "memeq128"                FoMemcmp.memeq128_bench
     ] 
 ]

 where
  go_all nm f
   = bgroup nm
   [ bench "4" $ check2 f 4
   , bench "8" $ check2 f 8
   , bench "16" $ check2 f 16
   , bench "32" $ check2 f 32
   , bench "64" $ check2 f 64
   , bench "128" $ check2 f 128
   , bench "256" $ check2 f 256
   ]

  check2 f n
   = nf f n


bench_atoi :: Benchmark
bench_atoi
 = bgroup "String to Int64"
 [ go_all "v-1"                 FoAtoi.string_to_i64_bench
 , go_all "v-128"               FoAtoi.string_to_i64_v128_bench
 , go_all "stdlib"               FoAtoi.string_to_i64_std_bench
 ]

 where
  go_all nm f
   = bgroup nm
   $ fmap (\str -> bench (show str) $ nf f str)
   [ "0", "1", "-123", "123456", "-12345678", "1234567890", "-123456789012345", "1234567890123456789", "12345678901234567890"
   ]


{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wwarn #-}

import qualified Anemone.Foreign.FFI as FoFFI
import qualified Bench.Foreign.Memcmp as FoMemcmp

import           Criterion.Main
import           Criterion.Types (Config(..))

import           System.IO (IO)

import           P



main :: IO ()
main
 = defaultMainWith benchConfig
 [ bench_ffi
 , bench_memcmp
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


{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Anemone.Foreign.FFI as FoFFI

import           Criterion.Main
import           Criterion.Types (Config(..))

import           System.IO (IO)

import           P

main :: IO ()
main
 = defaultMainWith benchConfig
 [ bench_ffi
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
 [ bench "None (return unit)"       $ nfIO (return ())
 , bench "Pure (id 0)"              $ nfIO (return $ id (0 :: Int))
 , bench "Safe (ffi_id_safe)"       $ nfIO (return $ FoFFI.ffi_id_safe   0)
 , bench "Unsafe (ffi_id_unsafe)"   $ nfIO (return $ FoFFI.ffi_id_unsafe 0)
 ]


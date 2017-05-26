{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Anemone.Roundtrip where

import           Anemone.Parser
import           Anemone.Pretty

import           Control.Monad.ST (runST, ST)

import           Data.Array.ST (MArray, STUArray, newArray, readArray)
import           Data.Array.Unsafe (castSTUArray)
import qualified Data.ByteString.Char8 as Char8

import           Disorder.Core.Run (ExpectedTestSpeed(..), disorderCheckEnvAll)
import           Disorder.Jack (Property)
import           Disorder.Jack (gamble, property, arbitrary, counterexample)

import           P

import           System.IO (IO)


prop_roundtrip_double :: Property
prop_roundtrip_double =
  gamble arbitrary $ \(n :: Int64) ->
  gamble arbitrary $ \(x :: Double) ->
  let
    original =
      0 + x * fromIntegral n

    intermediate =
      renderDouble original

    Just (roundtrip, "") =
      parseDouble intermediate

    original_i64 =
      fromDouble original

    roundtrip_i64 =
      fromDouble roundtrip

    diff_i64 =
      abs (original_i64 - roundtrip_i64)
  in
    counterexample "" .
    counterexample "Roundtrip failed." .
    counterexample "" .
    counterexample "=== Original ===" .
    counterexample (show original) .
    counterexample "" .
    counterexample "=== Intermediate ===" .
    counterexample (Char8.unpack intermediate) .
    counterexample "" .
    counterexample "=== Roundtrip ===" .
    counterexample (show roundtrip) .
    counterexample "" .
    counterexample "=== Original (Int64) ===" .
    counterexample (show original_i64) .
    counterexample "" .
    counterexample "=== Roundtrip (Int64) ===" .
    counterexample (show roundtrip_i64) .
    counterexample "" .
    counterexample "=== Diff (Int64) ===" .
    counterexample (show diff_i64) .
    property $
      diff_i64 <= 1

fromDouble :: Double -> Int64
fromDouble x =
  runST (cast x)

cast ::
     MArray (STUArray s) a (ST s)
  => MArray (STUArray s) b (ST s)
  => a
  -> ST s b
cast x =
   flip readArray 0 =<< castSTUArray =<< newArray (0 :: Int, 0) x
{-# INLINE cast #-}

return []
tests :: IO Bool
tests =
  $disorderCheckEnvAll TestRunMore

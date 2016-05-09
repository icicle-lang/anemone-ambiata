{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.Anemone.Foreign.Memcmp where

import qualified  Anemone.Foreign.Memcmp.Base   as Base
import qualified  Anemone.Foreign.Memcmp.Export as Export

import            P
import            Disorder.Core
import            Test.QuickCheck
import            Test.QuickCheck.Instances()

import qualified  Data.ByteString as B

testCmp'  :: (Eq a, Show a)
          => (B.ByteString -> B.ByteString -> a)
          -> (B.ByteString -> B.ByteString -> a)
          -> B.ByteString -> B.ByteString -> Property
testCmp' good check a b
 = let a' = pad a
       b' = pad b
   in  counterexample (show (a',b'))
       ( (good a' b') === check a' b')
 where
  len = min (B.length a) (B.length b)
  pad x = B.take len x


testCmp :: Base.MemcmpT -> B.ByteString -> B.ByteString -> Property
testCmp = testCmp' compare

testEq :: Base.MemeqT -> B.ByteString -> B.ByteString -> Property
testEq = testCmp' (==)

prop_memcmp8
 = testCmp Export.memcmp8
prop_memcmp64
 = testCmp Export.memcmp64
prop_memcmp128
 = testCmp Export.memcmp128
prop_memcmp
 = testCmp Export.memcmp

prop_memeq8
 = testEq Export.memeq8
prop_memeq64
 = testEq Export.memeq64
prop_memeq128
 = testEq Export.memeq128
prop_memeq
 = testEq Export.memeq


return []
tests = $disorderCheckEnvAll TestRunMore

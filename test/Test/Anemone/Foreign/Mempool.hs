{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.Anemone.Foreign.Mempool where

import Anemone.Foreign.Data

import            P
import            Disorder.Core
import            Test.QuickCheck
import            Test.QuickCheck.Instances()

foreign import ccall unsafe
    test_mempool_free
    :: CInt -> CInt -> CInt -> CBool

prop_mempool_free :: Property
prop_mempool_free
 = forAll iterations $ \a ->
   forAll iterations $ \b ->
   forAll megabytes $ \c ->
   test_mempool_free a b c /= 0

foreign import ccall unsafe
    test_mempool_nonoverlap
    :: CInt -> CInt -> CBool

prop_mempool_nonoverlap :: Property
prop_mempool_nonoverlap
 = forAll iterations $ \a ->
   forAll megabytes $ \b ->
   test_mempool_nonoverlap a b /= 0


iterations :: Gen CInt
iterations = choose (1, 100)

-- Choose some number of bytes from 1 to 2mb
-- (2mb is larger than block size)
megabytes :: Gen CInt
megabytes = choose (0, 2 * 1024 * 1024)

return []
tests = $disorderCheckEnvAll TestRunNormal


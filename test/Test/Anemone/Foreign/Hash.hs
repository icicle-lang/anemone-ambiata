{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Anemone.Foreign.Hash where

import           Anemone.Foreign.Hash

import           P

import           Test.QuickCheck (forAllProperties, quickCheckWithResult)
import           Test.QuickCheck (stdArgs, maxSuccess)
import           Test.QuickCheck ((===))
import           Test.QuickCheck.Instances ()


prop_fasthash32 bs =
  fasthash32' defaultSeed bs === fasthash32 bs

prop_fasthash64 bs =
  fasthash64' defaultSeed bs === fasthash64 bs

return []
tests =
  $forAllProperties $ quickCheckWithResult (stdArgs {maxSuccess = 10000})

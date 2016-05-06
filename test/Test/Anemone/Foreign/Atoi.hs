{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.Anemone.Foreign.Atoi where

import qualified  Anemone.Foreign.Atoi as Atoi

import            P
import            Disorder.Core
import            Test.QuickCheck
import            Test.QuickCheck.Instances()

import qualified  Data.ByteString       as B
import qualified  Data.ByteString.Char8 as BC

import Data.Char (isSpace)


testAtoi  :: Atoi.AtoiT
          -> B.ByteString -> Property
testAtoi check a
 = let a' = pad a
       r  = readMaybe' (BC.unpack a')
   in  counterexample (show (a'))
       (r === check a')
 where
  -- Pad it with some stuff at the end, so we do not segfault.
  pad x = x --  <> " A2345678123456781234567812345678"

  -- Our atoi does not remove leading whitespace
  readMaybe' (x:_)
   | isSpace x
   = Nothing
  readMaybe' x
   = case reads x of
      (v,_) : _ -> Just v
      _ -> Nothing

prop_atoi_scalar
 = testAtoi Atoi.atoi_scalar

zprop_atoi_vector128
 = testAtoi Atoi.atoi_vector128


return []
tests = $disorderCheckEnvAll TestRunMore

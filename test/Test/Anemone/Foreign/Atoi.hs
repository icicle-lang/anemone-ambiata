{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.Anemone.Foreign.Atoi where

import qualified  Anemone.Foreign.Atoi as Atoi

import            P
import            Disorder.Core
import            Test.QuickCheck
import            Test.QuickCheck.Instances()

import qualified  Data.ByteString       as B
import qualified  Data.ByteString.Char8 as BC

import Data.Char (isDigit)


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
  readMaybe' ('-':xs)
   = fmap negate $ readDigits 0 xs
  readMaybe' xs
   = readDigits 0 xs

  readDigits i (x:xs)
   | isDigit x
   , Just d <- readMaybe [x]
   = let i' = i * 10 + d
     in  case readDigits i' xs of
          Nothing -> Just i'
          Just d' -> Just d'
   | otherwise
   = Nothing
  readDigits _ []
   = Nothing

prop_atoi_scalar
 = testAtoi Atoi.atoi_scalar

prop_atoi_vector128
 = testAtoi Atoi.atoi_vector128


return []
tests = $disorderCheckEnvAll TestRunMore

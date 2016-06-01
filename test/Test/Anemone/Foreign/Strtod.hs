{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.Anemone.Foreign.Strtod where

import qualified  Anemone.Foreign.Strtod as Strtod

import            P
import            Disorder.Core
import            Test.QuickCheck
import            Test.QuickCheck.Instances()

import qualified  Data.ByteString       as B
import qualified  Data.ByteString.Char8 as BC

import Data.Char (isDigit)

import qualified Text.Read          as Read



testStrtodOnInts  :: Strtod.StrtodT
          -> B.ByteString -> Property
testStrtodOnInts check a
 = let mk8Dot = 46
       mk8e   = 101
       mk8E   = 69
       a' = B.filter (\c -> c /= mk8Dot && c /= mk8e && c /= mk8E) a
       r  = readMaybe' (BC.unpack a')
   in  (r === check a')
 where
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


prop_strtod_on_ints
 = testStrtodOnInts Strtod.strtod


testStrtodWellformed :: Strtod.StrtodT
          -> [Char] -> Property
testStrtodWellformed check a
 = let r  = Read.readMaybe a -- maybeOfEither (TextR.rational a)
       bs = BC.pack a
   in  (r === check bs)

prop_strtod_wellformed
 = forAll genWellformed
 $ testStrtodWellformed Strtod.strtod

genWellformed :: Gen [Char]
genWellformed
 = oneof [int, float]
 where
  num i
   = do i' <- elements [0..i]
        vectorOf i' (elements ['0'..'9'])

  int
   = do i <- num 18
        e <- expo
        return (i <> e)

  float
   = do i <- num 5
        f <- num 5
        e <- expo
        return (i <> "." <> f <> e)
  expo
   = oneof [(("e" <>) <$> num 3), return ""]


return []
tests = $disorderCheckEnvAll TestRunMore

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.Anemone.Foreign.Strtod where

import qualified  Anemone.Foreign.Strtod as Strtod
import qualified  Anemone.Foreign.Segv   as Segv

import            P
import            Disorder.Core
import            Disorder.Core.IO
import            Test.QuickCheck
import            Test.QuickCheck.Instances()

import qualified  Data.ByteString       as B
import qualified  Data.ByteString.Char8 as BC

import Data.Char (isDigit)
import Data.List (replicate, takeWhile)

import qualified Text.Read          as Read


strtod :: B.ByteString -> Maybe Double
strtod = fmap fst . Strtod.strtod

testStrtodOnInts  :: B.ByteString -> Property
testStrtodOnInts a
 = let mk8Dot = 46
       mk8e   = 101
       mk8E   = 69
       a' = B.filter (\c -> c /= mk8Dot && c /= mk8e && c /= mk8E) a
       r  = readMaybe' (BC.unpack a')
   in  (r === strtod a')
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

withSegv' f a
 = testIO $ Segv.withSegv (show a) (return $ f a)

prop_strtod_on_ints
 = withSegv'
 $ testStrtodOnInts


testStrtodWellformed :: [Char] -> Property
testStrtodWellformed a
 = let r  = readIgnoreInfinite a
       bs = BC.pack a
   in (r ~~~ strtod bs)
 where
 -- There is a bug where zero with a very large exponent becomes Infinite!
 --
 -- > > read "0e12345678901234567890" :: Double
 -- > Infinity
 --
 -- So what else? Strip the stuff before the 'e' and check if it's zero. If it is, return zero.
  readIgnoreInfinite ss
   = case Read.readMaybe ss of
      Just dbl
       | isInfinite dbl
       , Just zero <- Read.readMaybe (takeWhile (\c -> c /= 'e' && c /= 'E') ss)
       , zero == (0 :: Double)
       -> return 0
       | otherwise
       -> return dbl
      Nothing
       -> Nothing

prop_strtod_wellformed
 = forAll (genWellformed 19 19 19)
 $ withSegv'
 $ testStrtodWellformed

prop_strtod_length_20_1_1
 = forAll (genWellformed 20 1 1)
 $ withSegv'
 $ testStrtodWellformed

prop_strtod_length_1_20_1
 = forAll (genWellformed 1 20 1)
 $ withSegv'
 $ testStrtodWellformed

prop_strtod_length_1_1_20
 = forAll (genWellformed 1 1 20)
 $ withSegv'
 $ testStrtodWellformed


prop_strtod_length_20_20_3
 = forAll (genWellformed 20 20 3)
 $ withSegv'
 $ testStrtodWellformed

prop_strtod_length_40_1_1
 = forAll (genWellformed 40 1 1)
 $ withSegv'
 $ testStrtodWellformed

prop_strtod_length_1_40_1
 = forAll (genWellformed 1 40 1)
 $ withSegv'
 $ testStrtodWellformed

prop_strtod_length_40_40_3
 = forAll (genWellformed 40 40 3)
 $ withSegv'
 $ testStrtodWellformed

prop_strtod_length_80_80_10
 = forAll (genWellformed 80 80 10)
 $ withSegv'
 $ testStrtodWellformed

-- Test with a lots of zeros on the start.
-- This could cause precision issues
prop_strtod_length_20_20_3_zeroprefix
 = forAll (genWellformed 20 20 3) $ \dbl ->
   forAll (choose (0,20))         $ \zeros ->
   withSegv' testStrtodWellformed (replicate zeros '0' <> dbl)

-- Test with whitespace padding on the end
prop_strtod_length_20_20_3_suffix
 = forAll (genWellformed 20 20 3) $ \dbl ->
   forAll (choose (0,50))         $ \zeros ->
   withSegv' testStrtodWellformed (dbl <> replicate zeros ' ')




prop_strtod_double :: Double -> Property
prop_strtod_double
 = withSegv' (testStrtodWellformed . show)

-- strtod cannot parse all things that read can.
-- If the integral or fractional part requires more precision than
-- can be stored in a uint64 (and therefore more than an exact double),
-- it results in a parse error.
--
-- > λ read "12345678901234567890" :: Double
-- > 1.2345678901234567e19
-- > λ strtodPadded  "12345678901234567890"
-- > Nothing
--
genWellformed :: Int -> Int -> Int -> Gen [Char]
genWellformed num_int num_frac num_exp
 = oneof [int, float]
 where
  num i
   = do i' <- elements [1..i]
        vectorOf i' (elements ['0'..'9'])

  int
   = do i <- num num_int
        e <- expo
        return (i <> e)

  float
   = do i <- num num_int
        f <- num num_frac
        e <- expo
        return (i <> "." <> f <> e)
  expo
   = oneof [(("e" <>) <$> num num_exp), return ""]

return []
tests = $disorderCheckEnvAll TestRunMore

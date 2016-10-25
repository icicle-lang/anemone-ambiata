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
 = let r  = Read.readMaybe a
       bs = BC.pack a
   in (r ~~~ strtod bs)

prop_strtod_wellformed
 = forAll genWellformed
 $ withSegv' 
 $ testStrtodWellformed

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
genWellformed :: Gen [Char]
genWellformed
 = oneof [int, float]
 where
  num i
   = do i' <- elements [1..i]
        vectorOf i' (elements ['0'..'9'])

  int
   = do i <- num 19
        e <- expo
        return (i <> e)

  float
   = do i <- num 19
        f <- num 19
        e <- expo
        return (i <> "." <> f <> e)
  expo
   = oneof [(("e" <>) <$> num 3), return ""]

return []
tests = $disorderCheckEnvAll TestRunMore

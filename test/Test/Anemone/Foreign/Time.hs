{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Anemone.Foreign.Time where

import           Anemone.Foreign.Time

import qualified Data.ByteString.Char8 as Char8
import           Data.Thyme.Calendar (YearMonthDay(..), gregorianValid)

import           Disorder.Jack (Property)
import           Disorder.Jack ((===), gamble, discard, arbitrary, choose, listOf)
import           Disorder.Jack (forAllProperties, quickCheckWithResult)
import           Disorder.Jack (stdArgs, maxSuccess)

import           P

import           Text.Printf (printf)


prop_parseDate :: Property
prop_parseDate =
  gamble (choose (0, 9999)) $ \y ->
  gamble (choose (1, 12)) $ \m ->
  gamble (choose (1, 31)) $ \d ->
  gamble (listOf arbitrary) $ \xs ->
    case gregorianValid $ YearMonthDay y m d of
      Nothing ->
        discard
      Just day ->
        let
          str =
            Char8.pack $ printf "%04d-%02d-%02d%s" y m d xs
        in
          parseDate str === Just (day, Char8.pack xs)

return []
tests =
  $forAllProperties $ quickCheckWithResult (stdArgs {maxSuccess = 10000}) -- 10k certified

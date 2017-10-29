{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Anemone.Foreign.Time where

import           Anemone.Foreign.Time

import qualified Data.ByteString.Char8 as Char8
import           Data.Thyme.Calendar (Day(..), YearMonthDay(..), gregorianValid)

import           Disorder.Core.Run (ExpectedTestSpeed(..), disorderCheckEnvAll)
import           Disorder.Jack (Property)
import           Disorder.Jack ((===), gamble, arbitrary, choose, listOf)

import           P

import           Text.Printf (printf)


mkDay :: YearMonthDay -> Either TimeError Day
mkDay ymd =
  case gregorianValid ymd of
    Nothing ->
      Left $ TimeInvalidDate ymd
    Just day ->
      Right day

prop_parseYearMonthDay :: Property
prop_parseYearMonthDay =
  gamble (choose (0, 9999)) $ \y ->
  gamble (choose (1, 12)) $ \m ->
  gamble (choose (1, 31)) $ \d ->
  gamble (listOf arbitrary) $ \xs ->
    let
      ymd =
        YearMonthDay y m d

      eymd =
        fmap (const (ymd, Char8.pack xs)) $ mkDay ymd

      str =
        Char8.pack $ printf "%04d-%02d-%02d%s" y m d xs
    in
      parseYearMonthDay str === eymd

prop_parseDay :: Property
prop_parseDay =
  gamble (choose (0, 9999)) $ \y ->
  gamble (choose (1, 12)) $ \m ->
  gamble (choose (1, 31)) $ \d ->
  gamble (listOf arbitrary) $ \xs ->
    let
      ymd =
        YearMonthDay y m d

      eday =
        fmap (, Char8.pack xs) $ mkDay ymd

      str =
        Char8.pack $ printf "%04d-%02d-%02d%s" y m d xs
    in
      parseDay str === eday

prop_parseRenderError :: Property
prop_parseRenderError =
  gamble (choose (0, 9999)) $ \y ->
  gamble (choose (1, 12)) $ \m ->
  gamble (choose (1, 31)) $ \d ->
  gamble (listOf arbitrary) $ \xs ->
    let
      ymd =
        YearMonthDay y m d

      eymd =
        fmap (const (ymd, Char8.pack xs)) $ mkDay ymd

      str =
        Char8.pack $ printf "%04d-%02d-%02d%s" y m d xs
    in
      first renderTimeError (parseYearMonthDay str) === first renderTimeError eymd

return []
tests =
  $disorderCheckEnvAll TestRunMore

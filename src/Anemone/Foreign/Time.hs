{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Anemone.Foreign.Time (
    TimeError(..)
  , renderTimeError

  , parseDay
  , parseYearMonthDay
  ) where

import           Anemone.Foreign.Data

import           Data.Bits ((.&.), shiftR)
import qualified Data.ByteString as B
import           Data.ByteString.Internal (ByteString(..))
import qualified Data.Text as T
import           Data.Thyme.Calendar (YearMonthDay(..), Day(..))
import           Data.Word (Word8, Word64)

import           Foreign.ForeignPtr (withForeignPtr)
import           Foreign.Ptr (Ptr, plusPtr)

import           GHC.Generics (Generic)

import           P

import           System.IO (IO)
import           System.IO.Unsafe (unsafePerformIO)

import           Text.Printf (printf)


data TimeError =
    TimeParseError !ByteString
  | TimeInvalidDate !YearMonthDay
    deriving (Eq, Ord, Show, Generic)

instance NFData TimeError

renderTimeError :: TimeError -> Text
renderTimeError = \case
  TimeParseError bs0 ->
    let
      bs =
        if B.length bs0 > 20 then
          B.take 20 bs0 <> ".."
        else
          bs0
    in
      "Failed to parse date: " <> T.pack (show bs)
  TimeInvalidDate (YearMonthDay y m d) ->
    T.pack $
      printf "Parsed date was not in the gregorian calendar: %04d-%02d-%02" y m d

parseDay :: ByteString -> Either TimeError (Day, ByteString)
parseDay bs@(PS fp off len) =
  unsafePerformIO . withForeignPtr fp $ \p0 -> do
    let
      !p =
        p0 `plusPtr` off

      !pe =
        p `plusPtr` len

    !result <- c_parse_gregorian_as_modified_julian p pe

    let
      !mjd =
        fromIntegral $ (result .&. 0xFFFFFFFF00000000) `shiftAR` 32

      !err =
        fromIntegral $ (result .&. 0x00000000FFFFFFFF) :: CError

    case err of
      0 -> do
        pure $ Right (ModifiedJulianDay mjd, PS fp (off + 10) (len - 10))
      1 ->
        let
          -- if we get an invalid date error, then the payload is the y/m/d
          !year =
            fromIntegral $ (result .&. 0xFFFF000000000000) `shiftR` 48

          !month =
            fromIntegral $ (result .&. 0x0000FF0000000000) `shiftR` 40

          !day =
            fromIntegral $ (result .&. 0x000000FF00000000) `shiftR` 32
        in
          pure . Left $ TimeInvalidDate (YearMonthDay year month day)
      _ ->
        pure . Left $ TimeParseError bs

parseYearMonthDay :: ByteString -> Either TimeError (YearMonthDay, ByteString)
parseYearMonthDay bs@(PS fp off len) =
  unsafePerformIO . withForeignPtr fp $ \p0 -> do
    let
      !p =
        p0 `plusPtr` off

      !pe =
        p `plusPtr` len

    !result <- c_parse_gregorian p pe

    let
      !year =
        fromIntegral $ (result .&. 0xFFFF000000000000) `shiftR` 48

      !month =
        fromIntegral $ (result .&. 0x0000FF0000000000) `shiftR` 40

      !day =
        fromIntegral $ (result .&. 0x000000FF00000000) `shiftR` 32

      !err =
        fromIntegral $ (result .&. 0x00000000FFFFFFFF) :: CError

    case err of
      0 -> do
        pure $ Right (YearMonthDay year month day, PS fp (off + 10) (len - 10))
      1 ->
        pure . Left $ TimeInvalidDate (YearMonthDay year month day)
      _ ->
        pure . Left $ TimeParseError bs

-- | Do an arithmetic right shift on a 'Word64' (i.e. maintain the sign bit)
shiftAR :: Word64 -> Int -> Int64
shiftAR !x !n =
  fromIntegral x `shiftR` n :: Int64
{-# INLINE shiftAR #-}

foreign import ccall unsafe "anemone_parse_gregorian_hs"
  c_parse_gregorian :: Ptr Word8 -> Ptr Word8 -> IO Word64

foreign import ccall unsafe "anemone_parse_gregorian_as_modified_julian_hs"
  c_parse_gregorian_as_modified_julian :: Ptr Word8 -> Ptr Word8 -> IO Word64

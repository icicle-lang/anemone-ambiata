{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Anemone.Foreign.Time (
    parseDate
  ) where

import           Anemone.Foreign.Data

import           Data.ByteString.Internal (ByteString(..))
import           Data.Thyme.Calendar (Day(..))
import           Data.Word (Word8)

import           Foreign.ForeignPtr (withForeignPtr)
import           Foreign.Marshal.Alloc (alloca)
import           Foreign.Ptr (Ptr, plusPtr, minusPtr)
import           Foreign.Storable (peek, poke)

import           System.IO (IO)
import           System.IO.Unsafe (unsafePerformIO)

import           P


parseDate :: ByteString -> Maybe (Day, ByteString)
parseDate (PS fp off len) =
  unsafePerformIO . withForeignPtr fp $ \p0 ->
  alloca $ \pp ->
  alloca $ \pmjd -> do
    let
      !p =
        p0 `plusPtr` off

      !pe =
        p `plusPtr` len

    poke pp p
    !err <- c_string_to_mjd pp pe pmjd

    if err /= 0 then
      pure Nothing
    else do
      !p1 <- peek pp
      !mjd <- fromIntegral <$> peek pmjd

      let
        !off1 =
          p1 `minusPtr` p0

        !len1 =
          pe `minusPtr` p1

      pure $
        Just (ModifiedJulianDay mjd, PS fp off1 len1)

foreign import ccall unsafe "anemone_string_to_mjd"
  c_string_to_mjd :: Ptr (Ptr Word8) -> Ptr Word8 -> Ptr Int64 -> IO CError

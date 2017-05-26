{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
module Anemone.Foreign.Grisu2 (
    grisu2
  ) where

import           Anemone.Foreign.Data

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as ByteString
import           Data.Word (Word8)

import           Foreign.Ptr (Ptr)

import           System.IO (IO)

import           P


grisu2 :: Double -> ByteString
grisu2 x =
  ByteString.unsafeCreateUptoN maxDoubleLength $ \ptr ->
    fromIntegral <$> c_anemone_grisu2 x ptr
{-# INLINE grisu2 #-}

maxDoubleLength :: Int
maxDoubleLength =
  32

foreign import ccall unsafe "anemone_grisu2"
  c_anemone_grisu2 :: Double -> Ptr Word8 -> IO CSize

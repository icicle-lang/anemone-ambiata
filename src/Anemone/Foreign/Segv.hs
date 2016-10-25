{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BangPatterns #-}
module Anemone.Foreign.Segv (
    withSegv
  , segvInstall
  , segvRemove
  ) where

import           Control.Exception (bracket_)

import           Data.String (String)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Unsafe as B

import           Foreign.C.String (CString)

import           P

import           System.IO (IO)


withSegv :: String -> IO a -> IO a
withSegv example
 = bracket_ (segvInstall (B.pack example)) segvRemove

segvInstall :: B.ByteString -> IO ()
segvInstall a
 = B.unsafeUseAsCString a
 $ \a'
 -> anemone_segv_install_handler a' (B.length a)

foreign import ccall unsafe
    anemone_segv_install_handler
    :: CString -> Int -> IO ()

foreign import ccall unsafe
    "anemone_segv_remove_handler"
    segvRemove
    :: IO ()


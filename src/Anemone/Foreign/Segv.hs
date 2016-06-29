{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BangPatterns #-}
module Anemone.Foreign.Segv (
    withSegv
  , segvInstall
  , segvRemove
  ) where

import Foreign.C.String

import System.IO
import System.IO.Unsafe

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Unsafe as B

import P

withSegv :: Show a => (a -> b) -> a -> b
withSegv f a
 = unsafePerformIO
 $ do segvInstall (B.pack $ show a)
      let !b = f a
      segvRemove
      return b

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


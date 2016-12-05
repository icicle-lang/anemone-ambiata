{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Anemone.Foreign.Memcmp.Base (
    MemcmpT_Raw
  , wrapCmp
  ) where

import           Anemone.Foreign.Data

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B

import           Foreign.C.String (CString)

import           System.IO (IO)
import           System.IO.Unsafe (unsafePerformIO)

import           P

type MemcmpT_Raw
     = CString
    -> CString
    -> CSize
    -> IO CInt

{-# INLINE wrapCmp #-}
wrapCmp :: MemcmpT_Raw -> B.ByteString -> B.ByteString -> Ordering
wrapCmp f a b
 =  unsafePerformIO
 $  B.unsafeUseAsCString a
 $ \a'
 -> B.unsafeUseAsCString b
 $ \b'
 -> do  cmp <- f a' b' (fromIntegral $ min (B.length a) (B.length b))
        return (cmp `compare` 0)


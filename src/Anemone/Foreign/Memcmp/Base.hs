{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Anemone.Foreign.Memcmp.Base (
    MemcmpT, MemeqT, MemcmpT_Raw
  , wrapCmp
  , wrapEq

  ) where

import           Anemone.Foreign.Data

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B

import           Foreign.C.String (CString)

import           System.IO (IO)
import           System.IO.Unsafe (unsafePerformIO)

import           P

type MemcmpT = B.ByteString -> B.ByteString -> Ordering
type MemeqT  = B.ByteString -> B.ByteString -> Bool

type MemcmpT_Raw
     = CString
    -> CString
    -> CSize
    -> IO CInt

wrapCmp :: MemcmpT_Raw -> MemcmpT
wrapCmp f a b
 =  unsafePerformIO
 $  B.unsafeUseAsCString a
 $ \a'
 -> B.unsafeUseAsCString b
 $ \b'
 -> do  cmp <- f a' b' (fromIntegral $ min (B.length a) (B.length b))
        return (cmp `compare` 0)

wrapEq  :: MemcmpT_Raw -> MemeqT
wrapEq f a b
 = wrapCmp f a b == EQ


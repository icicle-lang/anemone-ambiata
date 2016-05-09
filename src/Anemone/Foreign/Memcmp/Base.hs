{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Anemone.Foreign.Memcmp.Base (
    MemcmpT, MemeqT, MemcmpT_Raw
  , wrapCmp
  , wrapEq
  ) where

import Foreign.C.String
import System.IO.Unsafe

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B

import P

type MemcmpT = B.ByteString -> B.ByteString -> Ordering
type MemeqT  = B.ByteString -> B.ByteString -> Bool

type MemcmpT_Raw
     = CString
    -> CString
    -> Int
    -> Int

wrapCmp :: MemcmpT_Raw -> MemcmpT
wrapCmp f a b
 =  unsafePerformIO
 $  B.unsafeUseAsCString a
 $ \a'
 -> B.unsafeUseAsCString b
 $ \b'
 -> let cmp = f a' b' (min (B.length a) (B.length b))
    in  return (cmp `compare` 0)

wrapEq  :: MemcmpT_Raw -> MemeqT
wrapEq f a b
 = wrapCmp f a b == EQ


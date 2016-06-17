{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
module Anemone.Foreign.Strtod (
    StrtodT
  , strtodUnsafe
  , strtodPadded
  ) where

import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable

import System.IO
import System.IO.Unsafe

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B

import P

strtodUnsafe :: StrtodT
strtodUnsafe bs
 = wrapStrtod anemone_strtod bs (B.length bs)

strtodPadded :: StrtodT
strtodPadded bs
 = wrapStrtod anemone_strtod (bs <> "ABCDABCDABCDABCD") (B.length bs)



type StrtodT = B.ByteString -> Maybe Double

type StrtodT_Raw
     = Ptr CString
    -> CString
    -> Ptr Double
    -> IO Bool

wrapStrtod :: StrtodT_Raw -> B.ByteString -> Int -> Maybe Double
wrapStrtod f a len
 =  unsafePerformIO
 $  B.unsafeUseAsCString a
 $ \a'
 -> alloca
 $ \a''
 -> alloca
 $ \ip
 -> do  let end = plusPtr a' len
        poke a'' a'
        suc   <- f a'' end ip
        res   <- peek ip
        return
             ( if   not suc
               then Just res
               else Nothing )



foreign import ccall unsafe
    anemone_strtod
    :: StrtodT_Raw



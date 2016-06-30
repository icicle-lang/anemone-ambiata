{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
module Anemone.Foreign.Strtod (
    StrtodT
  , strtod
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

strtod :: StrtodT
strtod bs
 = wrapStrtod anemone_strtod bs (B.length bs)




type StrtodT = B.ByteString -> Maybe (Double, B.ByteString)

type StrtodT_Raw
     = Ptr CString
    -> CString
    -> Ptr Double
    -> IO Bool

wrapStrtod :: StrtodT_Raw -> B.ByteString -> Int -> Maybe (Double, B.ByteString)
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
        end'  <- peek a''
        let diff = minusPtr end' a'
        let bs'  = B.drop diff a
        return
             ( if   not suc
               then Just (res, bs')
               else Nothing )



foreign import ccall unsafe
    anemone_strtod
    :: StrtodT_Raw



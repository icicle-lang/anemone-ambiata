{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ForeignFunctionInterface #-}
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
strtod
 = wrapStrtod anemone_strtod


type StrtodT = B.ByteString -> Maybe Double

type StrtodT_Raw
     = Ptr CString
    -> CString
    -> Ptr Double
    -> IO Bool

wrapStrtod :: StrtodT_Raw -> StrtodT
wrapStrtod f a
 =  unsafePerformIO
 $  B.unsafeUseAsCString a
 $ \a'
 -> alloca
 $ \a''
 -> alloca
 $ \ip
 -> do  let end = plusPtr a' (B.length a)
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



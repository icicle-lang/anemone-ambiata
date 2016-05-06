{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Anemone.Foreign.Atoi (
    AtoiT
  , atoi_scalar
  , atoi_vector128
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

atoi_scalar :: AtoiT
atoi_scalar
 = wrapAtoi anemone_string_to_i64

atoi_vector128 :: AtoiT
atoi_vector128
 = wrapAtoi anemone_string_to_i64_v128


type AtoiT = B.ByteString -> Maybe Int64

type AtoiT_Raw
     = Ptr CString
    -> CString
    -> Ptr Int64
    -> IO Bool

wrapAtoi :: AtoiT_Raw -> AtoiT
wrapAtoi f a
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
    anemone_string_to_i64
    :: AtoiT_Raw

foreign import ccall unsafe
    anemone_string_to_i64_v128
    :: AtoiT_Raw



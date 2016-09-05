{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Anemone.Foreign.Atoi (
    AtoiT
  , atoi
  , atoi_scalar
  , atoi_vector128
  ) where

import           Anemone.Foreign.Data

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B

import           Foreign.C.String (CString)
import           Foreign.Marshal.Alloc (alloca)
import           Foreign.Ptr (Ptr, plusPtr, minusPtr)
import           Foreign.Storable (peek, poke)

import           System.IO (IO)
import           System.IO.Unsafe (unsafePerformIO)

import           P

atoi :: B.ByteString -> Maybe (Int64, B.ByteString)
atoi bs
 = wrapAtoi anemone_string_to_i64_v128 bs (B.length bs)


atoi_scalar :: AtoiT
atoi_scalar bs
 = fmap fst
 $ wrapAtoi anemone_string_to_i64 bs (B.length bs)

atoi_vector128 :: AtoiT
atoi_vector128 = fmap fst . atoi


type AtoiT = B.ByteString -> Maybe Int64

type AtoiT_Raw
     = Ptr CString
    -> CString
    -> Ptr Int64
    -> IO CError

wrapAtoi :: AtoiT_Raw -> B.ByteString -> Int -> Maybe (Int64, B.ByteString)
wrapAtoi f a len
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
             ( if   suc == 0
               then Just (res, bs')
               else Nothing )



foreign import ccall unsafe
    anemone_string_to_i64
    :: AtoiT_Raw

foreign import ccall unsafe
    anemone_string_to_i64_v128
    :: AtoiT_Raw

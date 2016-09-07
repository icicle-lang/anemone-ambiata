{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Anemone.Foreign.VInt (
    bVInt

  , encodeVInt
  , encodeVIntArray

  , decodeVInt
  , decodeVIntArray
  ) where

import           Anemone.Foreign.Data

import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder.Prim as Builder
import qualified Data.ByteString.Builder.Prim.Internal as Builder
import           Data.ByteString.Internal (ByteString(..))
import qualified Data.ByteString.Internal as B
import qualified Data.Vector.Storable as Storable
import           Data.Word (Word8)

import           Foreign.C.Types (CInt(..))
import           Foreign.ForeignPtr (withForeignPtr)
import           Foreign.Marshal.Alloc (alloca)
import           Foreign.Ptr (Ptr, plusPtr, minusPtr)
import           Foreign.Storable (peek, poke)

import           GHC.ForeignPtr (mallocPlainForeignPtrBytes)

import           P

import           System.IO (IO)
import           System.IO.Unsafe (unsafePerformIO)


maxVIntSize :: Int
maxVIntSize =
  9

bVInt :: Int64 -> Builder
bVInt =
  Builder.primBounded $
    Builder.boudedPrim maxVIntSize c_write_vint

encodeVInt :: Int64 -> ByteString
encodeVInt x =
  unsafePerformIO $
    B.createUptoN maxVIntSize $ \ptr_out -> do
      !ptr_end <- c_write_vint x ptr_out
      pure $! ptr_end `minusPtr` ptr_out

encodeVIntArray :: Storable.Vector Int64 -> ByteString
encodeVIntArray xs =
  let
    n =
      Storable.length xs

    maxSize =
      maxVIntSize * n

    (fp_in, _) =
      Storable.unsafeToForeignPtr0 xs
  in
    unsafePerformIO $
      withForeignPtr fp_in $ \ptr_in ->
      B.createUptoN maxSize $ \ptr_out -> do
        !ptr_end <- c_write_vint_array (fromIntegral n) ptr_in ptr_out
        pure $! ptr_end `minusPtr` ptr_out

decodeVInt :: ByteString -> Maybe (Int64, ByteString)
decodeVInt (PS fp_in off_in len_in) =
  unsafePerformIO $ do
    (code, result, used) <-
      withForeignPtr fp_in $ \ptr_in0 ->
      alloca $ \pptr_in ->
      alloca $ \ptr_out -> do
        let
          !ptr_in1 =
            ptr_in0 `plusPtr` off_in

        poke pptr_in ptr_in1

        !code <-
          c_read_vint
            pptr_in
            (ptr_in1 `plusPtr` len_in)
            ptr_out

        ptr_in2 <- peek pptr_in
        out <- peek ptr_out

        pure $! (code, out, ptr_in2 `minusPtr` ptr_in1)

    case code of
      0 ->
        pure . Just $
          ( result
          , PS fp_in (off_in + used) (len_in - used)
          )
      _ ->
        pure Nothing

decodeVIntArray :: Int -> ByteString -> Maybe (Storable.Vector Int64, ByteString)
decodeVIntArray n (PS fp_in off_in len_in) =
  if n < 0 then
    Nothing
  else
    unsafePerformIO $ do
      fp_out <- mallocPlainForeignPtrBytes (n * 8)

      (code, used) <-
        withForeignPtr fp_out $ \ptr_out ->
        withForeignPtr fp_in $ \ptr_in0 ->
        alloca $ \pptr_in -> do
          let
            !ptr_in1 =
              ptr_in0 `plusPtr` off_in

          poke pptr_in ptr_in1

          !code <-
            c_read_vint_array
              pptr_in
              (ptr_in1 `plusPtr` len_in)
              (fromIntegral n)
              ptr_out

          ptr_in2 <- peek pptr_in

          pure $! (code, ptr_in2 `minusPtr` ptr_in1)

      case code of
        0 ->
          pure . Just $
            ( Storable.unsafeFromForeignPtr0 fp_out n
            , PS fp_in (off_in + used) (len_in - used)
            )
        _ ->
          pure Nothing

foreign import ccall unsafe "anemone_write_vint"
  c_write_vint ::
    Int64 ->
    Ptr Word8 ->
    IO (Ptr Word8)

foreign import ccall unsafe "anemone_write_vint_array"
  c_write_vint_array ::
    Int64 ->
    Ptr Int64 ->
    Ptr Word8 ->
    IO (Ptr Word8)

foreign import ccall unsafe "anemone_read_vint"
  c_read_vint ::
    Ptr (Ptr Word8) ->
    Ptr Word8 ->
    Ptr Int64 ->
    IO CError

foreign import ccall unsafe "anemone_read_vint_array"
  c_read_vint_array ::
    Ptr (Ptr Word8) ->
    Ptr Word8 ->
    Int64 ->
    Ptr Int64 ->
    IO CError

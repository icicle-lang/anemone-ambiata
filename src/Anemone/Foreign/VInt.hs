{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Anemone.Foreign.VInt (
    packVInt
  , unpackVInt
  ) where

import           Anemone.Foreign.Data

import           Data.ByteString.Internal (ByteString(..))
import qualified Data.ByteString.Internal as B
import qualified Data.Vector.Storable as Storable
import           Data.Word (Word8)

import           Foreign.C.Types (CInt(..))
import           Foreign.ForeignPtr (withForeignPtr)
import           Foreign.Ptr (Ptr, plusPtr)

import           GHC.ForeignPtr (mallocPlainForeignPtrBytes)

import           P

import           System.IO (IO)
import           System.IO.Unsafe (unsafePerformIO)


maxVIntSize :: Int
maxVIntSize =
  9

packVInt :: Storable.Vector Int64 -> ByteString
packVInt xs =
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
      B.createUptoN maxSize $ \ptr_out ->
        fromIntegral <$> c_pack_vint (fromIntegral n) ptr_in ptr_out

unpackVInt :: Int -> ByteString -> Maybe (Storable.Vector Int64)
unpackVInt n (PS fp_in off_in len_in) =
  if n < 0 then
    Nothing
  else
    unsafePerformIO $ do
      fp_out <- mallocPlainForeignPtrBytes (n * 8)

      code <-
        withForeignPtr fp_out $ \ptr_out ->
        withForeignPtr fp_in $ \ptr_in ->
          c_unpack_vint
            (fromIntegral n)
            (ptr_in `plusPtr` off_in)
            (ptr_in `plusPtr` off_in `plusPtr` len_in)
            ptr_out

      case code of
        0 ->
          pure . Just $
            Storable.unsafeFromForeignPtr0 fp_out n
        _ ->
          pure Nothing

foreign import ccall unsafe "anemone_pack_vint"
  c_pack_vint ::
    Int64 ->
    Ptr Int64 ->
    Ptr Word8 ->
    IO CSize

foreign import ccall unsafe "anemone_unpack_vint"
  c_unpack_vint ::
    Int64 ->
    Ptr Word8 ->
    Ptr Word8 ->
    Ptr Int64 ->
    IO CError

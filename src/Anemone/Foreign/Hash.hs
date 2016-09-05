{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Anemone.Foreign.Hash (
    defaultSeed

  , fasthash32
  , fasthash32'

  , fasthash64
  , fasthash64'
  ) where

import           Data.ByteString.Internal (ByteString(..))
import           Data.Word (Word8, Word32, Word64)

import           Foreign.C.Types (CSize(..))
import           Foreign.ForeignPtr (withForeignPtr)
import           Foreign.Ptr (Ptr, plusPtr)

import           P

import           System.IO (IO)
import           System.IO.Unsafe (unsafePerformIO)


defaultSeed :: Word64
defaultSeed =
  0x945ccb93eed24f3f 
{-# INLINE defaultSeed #-}

fasthash32 :: ByteString -> Word32
fasthash32 bs =
  fasthash32' defaultSeed bs
{-# INLINE fasthash32 #-}

fasthash32' :: Word64 -> ByteString -> Word32
fasthash32' seed (PS fp off len) =
  unsafePerformIO . withForeignPtr fp $ \ptr ->
    c_fashhash32 seed (ptr `plusPtr` off) (fromIntegral len)
{-# INLINE fasthash32' #-}

fasthash64 :: ByteString -> Word64
fasthash64 bs =
  fasthash64' defaultSeed bs
{-# INLINE fasthash64 #-}

fasthash64' :: Word64 -> ByteString -> Word64
fasthash64' seed (PS fp off len) =
  unsafePerformIO . withForeignPtr fp $ \ptr ->
    c_fashhash64 seed (ptr `plusPtr` off) (fromIntegral len)
{-# INLINE fasthash64' #-}

-- | uint32_t anemone_fasthash32 (uint64_t seed, const uint8_t *buf, size_t len);
foreign import ccall unsafe "anemone_fasthash32"
  c_fashhash32 :: Word64 -> Ptr Word8 -> CSize -> IO Word32

-- | uint64_t anemone_fasthash64 (uint64_t seed, const uint8_t *buf, size_t len);
foreign import ccall unsafe "anemone_fasthash64"
  c_fashhash64 :: Word64 -> Ptr Word8 -> CSize -> IO Word64

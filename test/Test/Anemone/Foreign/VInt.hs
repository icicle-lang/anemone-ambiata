{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Anemone.Foreign.VInt where

import           Anemone.Foreign.VInt

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Vector.Storable as Storable

import           Disorder.Core (ExpectedTestSpeed(..), disorderCheckEnvAll)
import           Disorder.Jack (Property, gamble)
import           Disorder.Jack (tripping, listOfN, sizedBounded, choose, arbitrary)

import           P

import qualified Prelude as Savage

import           System.IO (IO)

import           Test.QuickCheck.Instances ()


prop_roundtrip_vint_builder :: Property
prop_roundtrip_vint_builder =
  gamble sizedBounded $
    tripping (Lazy.toStrict . Builder.toLazyByteString . bVInt) (fmap noLeftovers . decodeVInt)

prop_roundtrip_vint :: Property
prop_roundtrip_vint =
  gamble sizedBounded $
    tripping encodeVInt (fmap noLeftovers . decodeVInt)

prop_roundtrip_vint_array :: Property
prop_roundtrip_vint_array =
  gamble (Storable.fromList <$> listOfN 1 1 sizedBounded) $ \xs ->
    tripping encodeVIntArray (fmap noLeftovers . decodeVIntArray (Storable.length xs)) xs

noLeftovers :: (a, ByteString) -> a
noLeftovers (xs, bs) =
  if B.null bs then
    xs
  else
    Savage.error $ "prop_roundtrip_vint: unexpected leftover bytes " <> show bs

xprop_unpack_safe :: Property
xprop_unpack_safe =
  gamble (choose (-1, 10000000)) $ \n ->
  gamble arbitrary $ \bs ->
    case decodeVIntArray n bs of
      Nothing ->
        True
      Just _ ->
        True

return []
tests :: IO Bool
tests =
  $disorderCheckEnvAll TestRunMore

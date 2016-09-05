{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Anemone.Foreign.VInt where

import           Anemone.Foreign.VInt

import qualified Data.Vector.Storable as Storable

import           Disorder.Core (ExpectedTestSpeed(..), disorderCheckEnvAll)
import           Disorder.Jack (Property, gamble)
import           Disorder.Jack (tripping, listOfN, sizedBounded, choose, arbitrary)

import           P

import           System.IO (IO)

import           Test.QuickCheck.Instances ()


prop_roundtrip_vint :: Property
prop_roundtrip_vint =
  gamble (Storable.fromList <$> listOfN 0 10000 sizedBounded) $ \xs ->
    tripping packVInt (unpackVInt $ Storable.length xs) xs

prop_unpack_safe :: Property
prop_unpack_safe =
  gamble (choose (-1, 10000000)) $ \n ->
  gamble arbitrary $ \bs ->
    case unpackVInt n bs of
      Nothing ->
        True
      Just _ ->
        True

return []
tests :: IO Bool
tests =
  $disorderCheckEnvAll TestRunMore

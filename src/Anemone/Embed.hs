{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Anemone.Embed (
    anemone_base_h
  , anemone_mempool_h
  , anemone_mempool_c
  , anemone_grisu2_h
  , anemone_grisu2_c
  ) where

import           Data.ByteString (ByteString)
import qualified Data.FileEmbed as FileEmbed


anemone_base_h :: ByteString
anemone_base_h =
  $(FileEmbed.embedFile "csrc/anemone_base.h")

anemone_mempool_h :: ByteString
anemone_mempool_h =
  $(FileEmbed.embedFile "csrc/anemone_mempool.h")

anemone_mempool_c :: ByteString
anemone_mempool_c =
  $(FileEmbed.embedFile "csrc/anemone_mempool.c")

anemone_grisu2_h :: ByteString
anemone_grisu2_h =
  $(FileEmbed.embedFile "csrc/anemone_grisu2.h")

anemone_grisu2_c :: ByteString
anemone_grisu2_c =
  $(FileEmbed.embedFile "csrc/anemone_grisu2.c")

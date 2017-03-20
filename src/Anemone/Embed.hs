{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Anemone.Embed
  ( anemone_base_h
  , anemone_mempool_h
  , anemone_mempool_c
  ) where

import qualified Data.FileEmbed as FileEmbed
import           Data.ByteString (ByteString)

anemone_base_h :: ByteString
anemone_base_h = $(FileEmbed.embedFile "csrc/anemone_base.h")

anemone_mempool_h :: ByteString
anemone_mempool_h = $(FileEmbed.embedFile "csrc/anemone_mempool.h")

anemone_mempool_c :: ByteString
anemone_mempool_c = $(FileEmbed.embedFile "csrc/anemone_mempool.c")

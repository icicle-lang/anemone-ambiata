{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Anemone.Foreign.Data (
    CBool(..)
  , CError(..)

  -- ** re-exports to avoid 'Unacceptable result type in foreign declaration'
  , CInt(..)
  , CSize(..)
  ) where

import           Data.Bits (Bits, FiniteBits)

import           Foreign.C.Types (CInt(..), CSize(..))
import           Foreign.Storable (Storable)

import           P

newtype CBool =
  CBool CInt
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Integral, Num, Real, FiniteBits, Bits, Storable)

newtype CError =
  CError CInt
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Integral, Num, Real, FiniteBits, Bits, Storable)

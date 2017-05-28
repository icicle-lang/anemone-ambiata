{-# LANGUAGE NoImplicitPrelude #-}
module Anemone.Pretty (
    renderDouble
  ) where

import Anemone.Foreign.Grisu2

import Data.ByteString (ByteString)

import P

-- | Render a 64-bit floating point number using the Grisu 2 [1] algorithm.
--
--   1. Printing Floating-Point Numbers Quickly and Accurately with Integers
--      Florian Loitsch
--      http://www.cs.tufts.edu/~nr/cs257/archive/florian-loitsch/printf.pdf
--
renderDouble :: Double -> ByteString
renderDouble x =
  grisu2 x
{-# INLINE renderDouble #-}

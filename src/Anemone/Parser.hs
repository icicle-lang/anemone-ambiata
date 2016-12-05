{-# LANGUAGE NoImplicitPrelude #-}
-- | This module is a simplified interface to all of Anemone's fast parsing
--   functionality.
--
module Anemone.Parser (
    TimeError(..)
  , renderTimeError

  , parseInt64
  , parseDouble
  , parseDay
  , parseYearMonthDay
  ) where


import qualified Anemone.Foreign.Atoi as Foreign
import qualified Anemone.Foreign.Strtod as Foreign
import           Anemone.Foreign.Time (TimeError(..), renderTimeError)
import qualified Anemone.Foreign.Time as Foreign

import           Data.ByteString (ByteString)
import           Data.Thyme (Day, YearMonthDay)

import           P

-- | Parse a signed decimal integer as an 'Int64'.
--
parseInt64 :: ByteString -> Maybe (Int64, ByteString)
parseInt64 =
  Foreign.atoi
{-# INLINE parseInt64 #-}

-- | Parse a rational number as a 'Double'.
--
parseDouble :: ByteString -> Maybe (Double, ByteString)
parseDouble =
  Foreign.strtod
{-# INLINE parseDouble #-}

-- | Parse a date in the format @YYYY-MM-DD@ as a 'Day'.
--
--   Note: if the date is formatted properly, but is not a valid date in the
--   gregorian calendar, then @Left (TimeInvalidDate _)@ is returned.
--
parseDay :: ByteString -> Either TimeError (Day, ByteString)
parseDay =
  Foreign.parseDay
{-# INLINE parseDay #-}

-- | Parse a date in the format @YYYY-MM-DD@ as a 'YearMonthDay'.
--
--   Note: if the date is formatted properly, but is not a valid date in the
--   gregorian calendar, then @Left (TimeInvalidDate _)@ is returned.
--
parseYearMonthDay :: ByteString -> Either TimeError (YearMonthDay, ByteString)
parseYearMonthDay =
  Foreign.parseYearMonthDay
{-# INLINE parseYearMonthDay #-}

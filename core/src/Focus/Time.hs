{-# LANGUAGE CPP #-}
module Focus.Time where

import Data.Time
import System.Locale hiding (defaultTimeLocale)
import Data.Time.Format hiding (defaultTimeLocale)
import Data.Time.Clock
import Data.Time.LocalTime.TimeZone.Series

formatTime' s tz t = formatTime defaultTimeLocale s $ ZoneSeriesTime t tz

showTime' = formatTime' "%l:%M%p"

showDateTime' = formatTime' "%D %l:%M%p"

maybeShowTime' tz s = maybe s (\t -> showTime' tz t)

dateTimeInputToUTCTime :: TimeZoneSeries -> String -> Maybe UTCTime
dateTimeInputToUTCTime tz s = fmap (zoneSeriesTimeToUTC . localTimeToZoneSeriesTime tz) $ parseTime defaultTimeLocale "%FT%k:%M" $ s

utcTimeToDateTimeInputValue :: TimeZoneSeries -> UTCTime -> String
utcTimeToDateTimeInputValue tz t = formatTime' "%Y-%m-%dT%R" tz t



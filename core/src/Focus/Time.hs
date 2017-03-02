{-# LANGUAGE CPP, DeriveLift, OverloadedStrings #-}
#ifdef USE_TEMPLATE_HASKELL
{-# LANGUAGE TemplateHaskell #-}
#endif
module Focus.Time where

import Data.Fixed
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Time.Calendar.WeekDate
import Data.Time.LocalTime.TimeZone.Series
import Focus.Misc
import Focus.Request
#if !defined(USE_TEMPLATE_HASKELL) || defined(DUMPING_SPLICES)
import Data.Aeson
#endif

import Focus.Schema

formatTime' :: Text -> TimeZoneSeries -> UTCTime -> Text
formatTime' s tz t = T.pack $ formatTime defaultTimeLocale (T.unpack s) $ ZoneSeriesTime t tz

showTime' :: TimeZoneSeries -> UTCTime -> Text
showTime' = formatTime' "%l:%M%p %Z"

showDateTime' :: TimeZoneSeries -> UTCTime -> Text
showDateTime' = formatTime' "%D %l:%M%p %Z"

maybeShowTime' :: TimeZoneSeries -> Text -> Maybe UTCTime -> Text
maybeShowTime' tz s = maybe s (\t -> showTime' tz t)

dateTimeInputToUTCTime :: TimeZoneSeries -> Text -> Maybe UTCTime
dateTimeInputToUTCTime tz s = fmap (zoneSeriesTimeToUTC . localTimeToZoneSeriesTime tz) $ parseTimeM True defaultTimeLocale "%FT%k:%M" $ T.unpack s

utcTimeToDateTimeInputValue :: TimeZoneSeries -> UTCTime -> Text
utcTimeToDateTimeInputValue tz t = formatTime' "%Y-%m-%dT%R" tz t

data Month = January
           | February
           | March
           | April
           | May
           | June
           | July
           | August
           | September
           | October
           | November
           | December
           deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance ShowPretty Month

data Meridian = AM
              | PM
              deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance ShowPretty Meridian

data WeekDay = Sunday
             | Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance ShowPretty WeekDay

type Year = Integer

weekDayToInt :: WeekDay -> Int
weekDayToInt = (\x -> if x == 0 then 7 else x) . fromEnum

addMonths :: Int -> (Integer, Month) -> (Integer, Month)
addMonths n (y, m) = (y + carry, toEnum m')
  where (carry, m') = (fromEnum m + n) `divMod'` 12

monthToInt :: Month -> Int
monthToInt = (+1) . fromEnum

intToMonth :: Int -> Month
intToMonth i = if i < 1 || i > 12 then error "intToMonth: Int out of range"
                                  else toEnum (i - 1)

intToWeekDay :: Int -> WeekDay
intToWeekDay i = if i < 1 || i > 7 then error "intToWeekDay: Int out of range"
                                   else if i == 7 then toEnum 0 else toEnum i

paddingZero :: (Num a, Ord a, Show a) => a -> Text
paddingZero n = if n >= 0 && n <10 then "0" <> T.pack (show n) else T.pack (show n)

dayToMonth :: Day -> Month
dayToMonth = intToMonth . snd3 . toGregorian

dayToDate :: Day -> Int
dayToDate = thd3 . toGregorian

dayToYear :: Day -> Integer
dayToYear = fst3 . toGregorian

setDayDate :: Int -> Day -> Day
setDayDate i = (\(y, m, _) -> fromGregorian y m i) . toGregorian

dayToMonthLength :: Day -> Int
dayToMonthLength = (\(y, m, _) -> gregorianMonthLength y m) . toGregorian

dayToWeekDay :: Day -> WeekDay
dayToWeekDay = intToWeekDay . thd3 . toWeekDate

localTimeToTwelveHourTime :: TimeOfDay -> (Int, Int, Meridian)
localTimeToTwelveHourTime tod =
  let h = todHour tod
      m = todMin tod
  in if h > 12 then (h - 12, m, PM) else if h == 0 then (12, m, AM) else (h, m, AM)

twelveHourTimeToLocalTime :: (Int, Int, Meridian) -> TimeOfDay
twelveHourTimeToLocalTime (h, m, meridian) = case meridian of
                                                  AM -> TimeOfDay (if h == 12 then 0 else h) m 0
                                                  PM -> TimeOfDay (if h == 12 then 12 else h + 12) m 0


newtype TimeZoneName = TimeZoneName { unTimeZoneName :: Text }
  deriving (Show, Read, Eq, Ord)

instance ShowPretty TimeZoneName where
  showPretty = unTimeZoneName

#ifdef USE_TEMPLATE_HASKELL
makeJson ''TimeZoneName
#else
instance ToJSON TimeZoneName where
  toJSON r_a2yM5
    = case r_a2yM5 of {
        TimeZoneName f_a2yM6
          -> toJSON ("TimeZoneName" :: String, toJSON (HCons f_a2yM6 HNil)) }
instance FromJSON TimeZoneName where
  parseJSON v_a2yM8
    = do { (tag'_a2yM9, v'_a2yMa) <- parseJSON v_a2yM8;
           case tag'_a2yM9 :: String of
             "TimeZoneName"
               -> do { HCons f_a2yMb HNil <- parseJSON v'_a2yMa;
                       return (TimeZoneName f_a2yMb) }
             _ -> fail "invalid message" }
#endif

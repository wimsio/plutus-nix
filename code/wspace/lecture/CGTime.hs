{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module CGTime
  ( utcToPOSIX
  , iso8601ToPOSIX
  , posixToUTC
  , posixToISO8601
  , getUTCNow
  , getPOSIXNow
  , getISO8601Now
  , getTimeTriple
  , addSeconds
  , addDaysUTC
  , diffSeconds
  , addSecondsPOSIX
  , addDaysPOSIX
  , diffSecondsPOSIX
  , formatUTC
  , parseUTC
  , getLocalISO8601
  , utcTimeNowToPOSIXTime
  ) where

import           Data.Time                             (UTCTime, NominalDiffTime)
import           Data.Time.Clock                       (getCurrentTime, addUTCTime, diffUTCTime)
import           Data.Time.Clock.POSIX                 (POSIXTime, getPOSIXTime, utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import           Data.Time.Format.ISO8601              (iso8601Show, iso8601ParseM)
import           Data.Time.Format                      (formatTime, parseTimeM, defaultTimeLocale)
import           Data.Time.LocalTime                   (getZonedTime)

-- | Convert a UTCTime to POSIXTime (seconds since 1970‑01‑01 UTC).
utcToPOSIX :: UTCTime -> POSIXTime
utcToPOSIX = utcTimeToPOSIXSeconds

-- | Parse an ISO‑8601 timestamp (e.g. "2025-05-04T14:30:00Z") and convert it to POSIXTime.
-- Returns Nothing if parsing fails.
iso8601ToPOSIX :: String -> Maybe POSIXTime
iso8601ToPOSIX s = utcToPOSIX <$> iso8601ParseM s

-- | Convert a POSIXTime back to UTCTime.
posixToUTC :: POSIXTime -> UTCTime
posixToUTC = posixSecondsToUTCTime

-- | Format a POSIXTime as an ISO‑8601 string (e.g. "2025-05-04T14:30:00Z").
posixToISO8601 :: POSIXTime -> String
posixToISO8601 = iso8601Show . posixToUTC

-- | Get the current UTC time as 'UTCTime'.
getUTCNow :: IO UTCTime
getUTCNow = getCurrentTime

-- | Get the current UTC time, convert it to POSIXTime, and wrap it in Just.
utcTimeNowToPOSIXTime :: IO (Maybe POSIXTime)
utcTimeNowToPOSIXTime = do
    utc <- getUTCNow
    let secs = fromInteger (floor (utcToPOSIX utc)) :: POSIXTime
    pure (Just secs)

-- | Get the current time as 'POSIXTime' (seconds since Unix epoch).
getPOSIXNow :: IO POSIXTime
getPOSIXNow = getPOSIXTime

-- | Get the current time formatted as an ISO‑8601 'String'.
getISO8601Now :: IO String
getISO8601Now = iso8601Show <$> getCurrentTime

-- | Get a triple of (UTCTime, POSIXTime, ISO‑8601 String) for the current moment.
getTimeTriple :: IO (UTCTime, POSIXTime, String)
getTimeTriple = do
  utc   <- getCurrentTime
  ptime <- getPOSIXTime
  let iso = iso8601Show utc
  return (utc, ptime, iso)

-- | Add a number of seconds to a UTCTime.
addSeconds :: NominalDiffTime -> UTCTime -> UTCTime
addSeconds = addUTCTime

-- | Add a number of days to a UTCTime (days * 86400 seconds).
addDaysUTC :: Integer -> UTCTime -> UTCTime
addDaysUTC d = addUTCTime (fromInteger (d * 86400))

-- | Difference between two UTCTime values, in seconds.
diffSeconds :: UTCTime -> UTCTime -> NominalDiffTime
diffSeconds = diffUTCTime

-- | Add a number of seconds to a POSIXTime.
addSecondsPOSIX :: NominalDiffTime -> POSIXTime -> POSIXTime
addSecondsPOSIX = (+)

-- | Add a number of days to a POSIXTime (days * 86400 seconds).
addDaysPOSIX :: Integer -> POSIXTime -> POSIXTime
addDaysPOSIX d = addSecondsPOSIX (fromInteger (d * 86400))

-- | Difference between two POSIXTime values, in seconds.
diffSecondsPOSIX :: POSIXTime -> POSIXTime -> NominalDiffTime
diffSecondsPOSIX = (-)

-- | Format a UTCTime using a custom format string.
-- e.g., formatUTC "%F %T" utctime produces "2025-05-04 14:30:00".
formatUTC :: String -> UTCTime -> String
formatUTC fmt = formatTime defaultTimeLocale fmt

-- | Parse a UTCTime from a string using a custom format and default locale.
-- Returns Nothing if parsing fails.
parseUTC :: String -> String -> Maybe UTCTime
parseUTC fmt s = parseTimeM True defaultTimeLocale fmt s

-- | Get the current local time formatted as ISO‑8601.
-- Relies on your system’s local timezone.
getLocalISO8601 :: IO String
getLocalISO8601 = iso8601Show <$> getZonedTime


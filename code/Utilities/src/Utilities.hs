module Utilities
  ( module X, iso8601ToPOSIX
  ) where

import           Utilities.Conversions as X
import           Utilities.PlutusTx    as X
import           Utilities.Serialise   as X

import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.Time.Clock.POSIX   (POSIXTime, utcTimeToPOSIXSeconds)
import Prelude

utcToPOSIX :: UTCTime -> POSIXTime
utcToPOSIX = utcTimeToPOSIXSeconds

-- | Parse an ISO‑8601 timestamp (e.g. "2025-05-04T14:30:00Z") and convert it to POSIXTime.
-- Returns Nothing if parsing fails.
iso8601ToPOSIX :: String -> Maybe POSIXTime
iso8601ToPOSIX s = utcToPOSIX <$> iso8601ParseM s

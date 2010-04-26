-- |This module extends UTCTime with binary serialization.

module UTCTimeExts where

import Data.Binary
import Data.Time.Clock
import Data.Time.Calendar (fromGregorian)
import Control.Monad (liftM)

-- |Implicit serialisation of UTCTime, drops milliseconds silently.
instance Binary UTCTime where
    put d = put (toUnixSeconds d)
    get = liftM fromUnixSeconds get

unixEpoch = UTCTime (fromGregorian 1970 1 1) 0

-- |Converts UTCTime to UNIX time stamp seconds. Please note! This
-- truncates second fractions.
toUnixSeconds :: UTCTime -> Integer
toUnixSeconds d = truncate $ diffUTCTime d unixEpoch

fromUnixSeconds :: Integer -> UTCTime
fromUnixSeconds s = addUTCTime (fromInteger s) unixEpoch

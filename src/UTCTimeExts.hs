-- |This module extends UTCTime with binary serialization.

module UTCTimeExts where

import Data.Binary
import Data.Time.Clock
import Data.Time.Calendar
import Control.Monad (liftM)
import Control.DeepSeq

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

-- Unix timestamp units
hourLen = 60*60
dayLen = 24*hourLen
weekLen = 7*dayLen

hour :: UTCTime -> Integer
hour d = (toUnixSeconds d) `mod` dayLen `div` hourLen

dayOfWeek :: UTCTime -> Integer
dayOfWeek d = fixDay $ (toUnixSeconds d) `mod` weekLen `div` dayLen

fixDay unix = (unix + 3) `mod` 7 + 1


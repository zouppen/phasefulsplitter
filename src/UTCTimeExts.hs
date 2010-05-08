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

-- |Little dummy parser.
instance NFData UTCTime where
    rnf x = rnf (toUnixSeconds x)

unixEpoch = UTCTime (fromGregorian 1970 1 1) 0

-- |Converts UTCTime to UNIX time stamp seconds. Please note! This
-- truncates second fractions.
toUnixSeconds :: UTCTime -> Integer
toUnixSeconds d = truncate $ diffUTCTime d unixEpoch

fromUnixSeconds :: Integer -> UTCTime
fromUnixSeconds s = addUTCTime (fromInteger s) unixEpoch

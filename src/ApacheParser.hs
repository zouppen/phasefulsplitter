module ApacheParser where

import qualified Data.ByteString.Lazy.Char8 as B
import Text.Regex.TDFA.ByteString.Lazy
import Text.Regex.TDFA.Common
import Data.Time.Clock
import Data.Time.Format
import System.Locale
import Network.URL
import Control.Monad (liftM,ap)

import qualified LineInfo as L
import qualified Entry as E
import RegexHelpers

-- |Converts a bytestring containing an Apache date to UTCTime
fromApacheTime :: B.ByteString -> Maybe UTCTime
fromApacheTime s = parseTime defaultTimeLocale "%d/%b/%Y:%T %z" $ B.unpack s

-- |Apache style log file format. The fourth group is quite messy
-- because [^]] is broken in TDFA (or it is broken in grep).
apacheLogRegex = compileString "^([0-9\\.]+) ([^ ]+) +([^ ]+) +\\[([A-Za-z0-9 +-:/]*)\\] \"([^\"]*)\" ([0-9]{3}) ([0-9]+|-) \"([^\"]*)\" \"([^\n]*)\"$"

-- |Reads an entry from a given ByteString. Returns Nothing in case of
-- |an error. Error type is not propagated.
getEntry :: (L.LineInfo,B.ByteString) -> (Maybe E.Entry)
getEntry (info,line) = splitBS apacheLogRegex line >>=
                       toEntry info

-- |Creates an entry from regex match groups and pushes Maybe to the
-- front of Entry if one of the operations does fail.
toEntry :: L.LineInfo -> [B.ByteString] -> Maybe E.Entry
toEntry info [rawIP,_,_,rawDate,rawReq,rawResponse,rawBytes,rawReferer,rawBrowser] =
    (Just $ E.Entry info) 
    `ap` (Just rawIP) 
    `ap` (fromApacheTime rawDate)
    `ap` (liftM (!! 0) req) -- method
    `ap` (liftM (B.unpack . (!! 1)) req >>= importURL) -- URL
    `ap` (liftM (!! 2) req) -- protocol
    `ap` (readInteger rawResponse)
    `ap` (readInteger rawBytes)
    `ap` (Just rawReferer)
    `ap` (Just rawBrowser)
    where req = splitBS requestRegex rawReq

requestRegex = compileString "^([A-Z]+) (.+) ([^ ]+)"

-- |Reads integer from ByteString, failing if the entire ByteString is
-- not parsed as a string.
readInteger :: B.ByteString -> Maybe Integer
readInteger bs | bs == B.pack "-" = Just 0
readInteger bs = case (B.readInteger bs) of
                   Nothing -> Nothing
                   Just (i,rest) -> if rest == B.empty then Just i
                                    else Nothing


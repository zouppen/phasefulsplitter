module ApacheParser where

import qualified Data.ByteString.Lazy.Char8 as B
import Text.Regex.TDFA.ByteString.Lazy
import Text.Regex.TDFA.Common
import Data.Time.Clock
import Data.Time.Format
import System.Locale
import Network.URL
import Control.Monad (liftM,ap)

import qualified Entry as E
import RegexHelpers

-- |Converts a bytestring containing an Apache date to UTCTime
fromApacheTime :: B.ByteString -> Maybe UTCTime
fromApacheTime s = parseTime defaultTimeLocale "%d/%b/%Y:%T %z" $ B.unpack s

-- |Apache style log file format. The fourth group is quite messy
-- because [^]] is broken in TDFA (or it is broken in grep).
apacheLogRegex = compileString "^([0-9\\.]+) ([^ ]+) +([^ ]+) +\\[([A-Za-z0-9 +-:/]*)\\] \"([^\"]*)\" ([0-9]{3}) ([0-9]+|-) \"([^\"]*)\" \"([^\n]*)\"$"


-- |Reads an entry from a given ByteString. Returns Right with
-- resulting entry on success and Left in case of an error.
getEntry :: InLine -> Maybe Entry)
getEntry line = splitBS apacheLogRegex $ line_raw line >>=
                toEntry (server_id line) (line_n line)

-- |Creates an entry from regex match groups and pushes Maybe to the
-- front of Entry if one of the operations does fail.
toEntry :: [B.ByteString] -> Maybe Entry
toEntry server line [rawIP,_,_,rawDate,rawReq,rawResponse,rawBytes,rawReferer,rawBrowser] =
    (Just $ Entry server line) 
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

data InLine = InLine {
      server_id :: Integer
    , row       :: Integer
    , line_raw  :: B.ByteString
} deriving (Show,Eq)

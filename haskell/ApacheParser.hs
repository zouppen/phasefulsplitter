module ApacheParser where

import qualified Data.ByteString.Lazy.Char8 as B
import Text.Regex.TDFA.ByteString.Lazy
import Text.Regex.TDFA.Common
import Data.Time.Clock
import Data.Time.Format
import System.Locale
import Network.URL
import Control.Monad (liftM,ap)

import Entry

-- |Converts a bytestring containing an Apache date to UTCTime
fromApacheTime :: B.ByteString -> Maybe UTCTime
fromApacheTime s = parseTime defaultTimeLocale "%d/%b/%Y:%T %z" $ B.unpack s

-- |Apache style log file format. The fourth group is quite messy
-- because [^]] is broken in TDFA (or it is broken in grep).
apacheLogRegex = compileNicely "^([0-9\\.]+) ([^ ]+) +([^ ]+) +\\[([A-Za-z0-9 +-:/]*)\\] \"([^\"]*)\" ([0-9]{3}) ([0-9]+|-) \"([^\"]*)\" \"([^\n]*)\"\n"

-- |Compiles String into a TDFA Regex with good options.
compileNicely :: String -> Regex
compileNicely regexText = fromEither $ compile CompOption {caseSensitive = False, multiline = False, rightAssoc = True, newSyntax = True, lastStarGreedy = False} ExecOption {captureGroups = True} (B.pack regexText)

-- |Transforms "Either errors" to exception errors. Haskell has 8
-- types of errors, so this is only a minor help.
fromEither :: Either String t -> t
fromEither = either error id

-- |Reads an entry from a given ByteString. Returns result and the
-- rest of the ByteString as a pair. Used by unfoldr and therefore it
-- returns Just even if there will be parse error in that line. Parse
-- errors are marked with Left and processing will continue even if
-- parsing fails.
getEntry :: B.ByteString -> Maybe (Either (String,B.ByteString) Entry, B.ByteString)
getEntry bs | bs == B.empty = Nothing
            | otherwise     = Just $ getEntry' bs

getEntry' bs = case match of
                 Left msg -> error msg -- There is a fatal error in the pattern
                 Right Nothing -> (Left ("no match",curLine),nextLine)
                 Right (Just (_,_,rest,ms)) -> (eitherifyMaybe ("parse error",curLine) (toEntry ms),rest)
    where match = regexec apacheLogRegex bs
          curLine = B.takeWhile (/='\n') bs
          nextLine = B.tail $ B.dropWhile (/='\n') bs

-- |Creates an entry from regex match groups and pushes Maybe to the
-- front of Entry if one of the operations does fail.
toEntry :: [B.ByteString] -> Maybe Entry
toEntry [rawIP,_,_,rawDate,rawReq,rawResponse,rawBytes,rawReferer,rawBrowser] =
    (Just Entry) 
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

requestRegex = compileNicely "^([A-Z]+) (.+) ([^ ]+)"

-- |Splits a given bytestring to a list of groups with a Regex.
splitBS :: Regex -> B.ByteString -> Maybe [B.ByteString]
splitBS regex bs = case match of 
                   Left s -> Nothing
                   Right Nothing -> Nothing
                   Right (Just (_,_,_,ms)) -> Just ms
  where match = regexec regex bs

-- |Reads integer from ByteString, failing if the entire ByteString is
-- not parsed as a string.
readInteger :: B.ByteString -> Maybe Integer
readInteger bs | bs == B.pack "-" = Just 0
readInteger bs = case (B.readInteger bs) of
                   Nothing -> Nothing
                   Just (i,rest) -> if rest == B.empty then Just i
                                    else Nothing

-- |Stupid way to convert Nothing to Left.
eitherifyMaybe _ (Just x) = Right x
eitherifyMaybe err Nothing = Left err

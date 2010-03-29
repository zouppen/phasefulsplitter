module LogFileReader where

import Data.List
import Codec.Compression.GZip (decompress)
import qualified Data.ByteString.Lazy.Char8 as B
import Text.Regex.TDFA.ByteString.Lazy
import Text.Regex.TDFA.Common

-- |Apache style log file format. The fourth group is quite messy
-- because [^]] is broken in TDFA (or it is broken in grep).
apacheLogRegexText = "^([0-9\\.]+) ([^ ]+) +([^ ]+) +\\[([A-Za-z0-9 +-:/]*)\\] \"([^\"]*)\" ([0-9]{3}) ([0-9]+|-) \"(.*)\" \"(.*)\"$"

apacheLogRegex = fromEither $ compile CompOption {caseSensitive = False, multiline = True, rightAssoc = True, newSyntax = True, lastStarGreedy = False} ExecOption {captureGroups = True} (B.pack apacheLogRegexText)

-- |Transforms "Either errors" to exception errors. Haskell has 8
-- types of errors, so this is only a minor help.
fromEither :: Either String t -> t
fromEither = either error id

testMatch filePath = do
  fileData <- B.readFile filePath
  return $ unfoldr getEntry $ decompress fileData


-- |Reads an entry from a given ByteString. Returns result and the
-- rest of the ByteString as a pair.
getEntry :: B.ByteString -> Maybe (Either (String,B.ByteString) [B.ByteString], B.ByteString)
getEntry bs = if bs == B.empty then Nothing
              else Just $ getEntry' bs

getEntry' bs = case match of
                 Left msg -> (Left (msg,curLine),nextLine)
                 Right Nothing -> (Left ("no match",curLine),nextLine)
                 Right (Just (_,_,rest,ms)) -> (Right (entry ms),B.tail rest)
    where match = regexec apacheLogRegex bs
          curLine = B.takeWhile (/='\n') bs
          nextLine = B.tail $ B.dropWhile (/='\n') bs

entry = id

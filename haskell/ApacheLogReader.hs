module ApacheLogReader where

import ApacheParser (getEntry)

import Codec.Compression.GZip (decompress)
import qualified Data.ByteString.Lazy.Char8 as B
import Control.Parallel.Strategies
import System.IO
import Entry
import qualified LineInfo as L

readEntriesFromFile :: (L.LineInfo,FilePath) -> IO [Either (L.LineInfo,B.ByteString) Entry]
readEntriesFromFile (lineInfo,filePath) = do
  fileData <- B.readFile filePath
  return $ parMap rwhnf getEitheredEntry $ numLines fileData
  
    where inliner i bs = (lineInfo{L.lineNo = i},bs)
          numLines gzData = zipWith inliner [1..] $ B.lines $ decompress gzData

-- |Gets entry in "eithered" form. If parsing of 'blob' fails, then
-- |Left, otherwise return resilt in Right.
getEitheredEntry blob = case getEntry blob of
                          Nothing -> Left blob
                          Just a -> Right a

findErrors lineInfo filePath = do
  entries <- readEntriesFromFile (lineInfo,filePath)
  return $ head $ filter (not.codecOK) entries

-- Split the entry list, compute it with multiple processors and
-- combine results.
--  mergedEntries <- nmergeIO $ unmerge threads entries

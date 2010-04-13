module ApacheLogReader where

import ApacheParser (getEntry)

import Codec.Compression.GZip (decompress)
import qualified Data.ByteString.Lazy.Char8 as B
import System.IO
import Entry

readEntriesFromFile LineInfo -> FilePath -> IO (Either (LineInfo,B.ByteString) Entry)
readEntriesFromFile lineinfo filePath = do
  fileData <- B.readFile filePath
  return $ map getEitheredEntry $ zipWith inliner [1..] $ B.lines $ decompress fileData
    where inliner i bs = (lineinfo{linenum = i},bs)

getEitheredEntry blob = case getEntry blob of
                          Nothing -> Left blob
                          Just a -> Right a

-- Some ideas of compressing output, but it's difficult to run in parallel...
-- serialiseBads xs = unlines $ map show xs
-- serialiseGoods xs = compress $ concat $ map encode xs

findErrors filePath = do
  entries <- readEntriesFromFile (LineInfo ) filePath
  return $ head $ filter (not.codecOK) entries


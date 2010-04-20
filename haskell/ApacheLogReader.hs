module ApacheLogReader where

import ApacheParser (getEntry)

import Codec.Compression.GZip (decompress)
import qualified Data.ByteString.Lazy.Char8 as B
import System.IO
import Entry
import qualified LineInfo as L

readBlobsFromFile :: (L.LineInfo,FilePath) -> IO [(L.LineInfo,B.ByteString)]
readBlobsFromFile (lineInfo,filePath) = do
  fileData <- B.readFile filePath
  return $ zipWith inliner [1..] $ B.lines $ decompress fileData
    where inliner i bs = (lineInfo{L.lineNo = i},bs)


-- |Gets entry in "eithered" form. If parsing of 'blob' fails, then
-- |Left, otherwise return resilt in Right.
getEitheredEntry blob = case getEntry blob of
                          Nothing -> Left blob
                          Just a -> Right a

findErrors lineInfo filePath = do
  blobs <- readBlobsFromFile (lineInfo,filePath)
  return $ head $ filter (not.codecOK) $ map getEitheredEntry blobs

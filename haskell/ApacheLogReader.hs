module ApacheLogReader where

import ApacheParser (getEntry)

import Codec.Compression.GZip (decompress)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List (unfoldr)
import System.IO
import Entry

readEntriesFromFile server_id filePath = do
  fileData <- B.readFile filePath
  return $ map getEntry $ zipWith inliner [1..] $ B.lines $ decompress fileData
    where inliner = InLine server_id

-- Some ideas of compressing output, but it's difficult to run in parallel...
-- serialiseBads xs = unlines $ map show xs
-- serialiseGoods xs = compress $ concat $ map encode xs

findErrors filePath = do
  entries <- readEntriesFromFile filePath
  return $ head $ filter (not.codecOK) entries


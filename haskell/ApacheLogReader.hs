module ApacheLogReader where

import ApacheParser (getEntry)

import Codec.Compression.GZip (decompress)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List (unfoldr)

readEntriesFromFile filePath = do
  fileData <- B.readFile filePath
  return $ unfoldr getEntry $ decompress fileData
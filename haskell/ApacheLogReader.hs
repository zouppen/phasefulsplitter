module ApacheLogReader where

import ApacheParser (getEntry)

import Codec.Compression.GZip (decompress)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List (unfoldr)
import System.IO
import Data.Binary
import Entry

readEntriesFromFile filePath = do
  fileData <- B.readFile filePath
  return $ unfoldr getEntry $ decompress fileData

-- |Processes one file and write to a new file and an error log.
processFile fromFile toFile errorLogFile = do
  fileData <- B.readFile fromFile
  let entries = unfoldr getEntry $ decompress fileData
  
  toHandle <- openFile toFile WriteMode
  errHandle <- openFile errorLogFile AppendMode
  
  mapM_ (writeEntry toHandle errHandle) entries -- Write to either file
  
  hClose toHandle
  hClose errHandle

writeEntry :: (Show a) => Handle -> Handle -> Either a Entry -> IO ()
writeEntry outHandle _ (Right entry) =
    case testBinary entry of
      True -> B.hPut outHandle $ encode entry
      False -> error $ "Binary codec failed: " ++ show entry
writeEntry _ errHandle (Left err) = hPutStrLn errHandle $ show err

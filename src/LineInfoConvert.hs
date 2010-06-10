-- |Converts (file id, server id, line number) triplet back to a line
-- |in a log file.

module LineInfoConvert where

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map as M
import Data.List
import Control.Monad
import Codec.Compression.GZip
import LineInfo

type RawFileList = (String,[(LineInfo,String)])
type FileMap = M.Map LineInfo String

-- |Reads file list from a given file, discarding service name.
readRawFileList :: FilePath -> IO RawFileList
readRawFileList f = liftM read $ readFile f

-- |Converts raw file list into a more usable form.
toFileMap :: RawFileList -> FileMap
toFileMap raw = M.fromList $ snd raw

-- |Convenience function for doing the two above in one pass.
readFileMap :: FilePath -> IO FileMap
readFileMap f = liftM toFileMap $ readRawFileList f

-- |Very naive log file reader.
getLogLine :: FileMap -> LineInfo -> IO B.ByteString
getLogLine fMap lineInfo = do
  contents <- B.readFile fileName
  return $ genericIndex (B.lines $ decompress contents) line
    where fileInfo = lineInfo{lineNo = 0}
          fileName = (M.!) fMap fileInfo
          line = lineNo fileInfo
          
printLogLines :: FilePath -> [LineInfo] -> IO ()
printLogLines fileList xs = do
  fMap <- readFileMap fileList
  logLines <- mapM (getLogLine fMap) xs
  B.putStr $ B.unlines $ zipWith niceLineOutput xs logLines

-- |Formats LineInfo and a log file line into a nice looking ByteString.
niceLineOutput :: LineInfo -> B.ByteString -> B.ByteString
niceLineOutput (LineInfo a b c) line =
    B.intercalate (B.pack " ") [(toBS a),(toBS b),(toBS c),line]
    where toBS x = B.pack $ show x

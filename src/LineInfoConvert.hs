-- |Converts (file id, server id, line number) triplet back to a line
-- |in a log file.

module LineInfoConvert where

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map as M
import Data.List
import Data.List.Split
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
    where fileName = getFileName fMap lineInfo
          line = lineNo lineInfo - 1  -- LineInfo indices do start from one.
          
-- |Returns file name where that lineinfo is found.
getFileName :: FileMap -> LineInfo -> FilePath
getFileName fMap lineInfo = (M.!) fMap fileInfo
  where fileInfo = lineInfo{lineNo = 0}


getLogLines :: FilePath -> [LineInfo] -> IO [B.ByteString]
getLogLines fileList xs = do
  fMap <- readFileMap fileList
  logLines <- mapM (getLogLine fMap) xs
  return $ zipWith niceLineOutput xs logLines

printLogLines :: FilePath -> [LineInfo] -> IO ()
printLogLines fileList xs = do
  ls <- getLogLines fileList xs
  B.putStr $ B.unlines ls

-- |Writes anomalies to a given file.
saveLogLines :: FilePath -> FilePath -> [LineInfo] -> IO ()
saveLogLines fileList outFile xs = do
  ls <- getLogLines fileList xs
  B.writeFile outFile $ B.unlines ls

-- |Formats LineInfo and a log file line into a nice looking ByteString.
niceLineOutput :: LineInfo -> B.ByteString -> B.ByteString
niceLineOutput (LineInfo a b c) line =
    B.intercalate (B.pack " ") [(toBS a),(toBS b),(toBS c),line]
    where toBS x = B.pack $ show x

-- |"Zipola format" is Matlab text output I often get from my workmate.
readZipolaFormat :: String -> LineInfo
readZipolaFormat line = LineInfo (readI 0) (readI 1) (readI 2)
  where elems = split (dropInitBlank . condense . dropDelims $ oneOf ", \t") line
        readI i = read $ elems !! i

-- |Read an entire "Zipola formatted" file.
readZipolaFile :: FilePath -> IO [LineInfo]
readZipolaFile f = do
  contents <- readFile f
  return $ map readZipolaFormat $ filter (not.empty) $ lines contents

empty [] = True
empty ('#':_) = True
empty _ = False

-- |Backtracks a list of files. Takes in file list and infile-outfile pairs.
backtrackCSV :: FilePath -> [(FilePath,FilePath)] -> IO () 
backtrackCSV fileList target = do
  lineInfos <- mapM readZipolaFile inFiles
  mapM_ saveLogLines' $ zip outFiles lineInfos
  where inFiles = map fst target
        outFiles = map snd target
        saveLogLines' = uncurry (saveLogLines fileList)
        
backtrackCSVnumeric :: (Integral a) => FilePath -> FilePath -> FilePath -> [a] -> IO ()
backtrackCSVnumeric fileList inPrefix outPrefix resources = do
  backtrackCSV fileList $ zip inFiles outFiles
  where inFiles = map ((inPrefix++).show) resources
        outFiles = map ((outPrefix++).show) resources

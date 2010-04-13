module Main where

import qualified Data.ByteString.Lazy.Char8 as B
import System.IO
import System.Environment
import Data.Binary
import Control.Monad

import Entry
import RegexHelpers
import ApacheLogReader

-- |Converts a file into Haskell data structure (which can be gzipped
-- |and stored for long times at your disposal). 'fromFile' is the
-- |file to read and 'toBase is the base name of the file (appended
-- |with .0.pf and .error as needed.
convertFile fromFile toBase = do
  outH <- openFile (toBase ++ ".0.pf") WriteMode
  errH <- openFile (toBase ++ ".0.error.txt") WriteMode

  case splitBS fileRegex (B.pack fromFile) of
    Just [server,service] -> do
      -- Output file headers 
      B.hPut outH $ encode (server,service)
      entries <- readEntriesFromFile fromFile
      writeEntries outH errH entries
    _ -> hPutStrLn errH "fatal: File name pattern doesn't match."
  
  hClose outH
  hClose errH

-- |Writes entries to a given handle. Doesn't continue if the first
-- |line fails.
writeEntries :: (Show a) => Handle -> Handle -> [Either a Entry] -> IO ()
writeEntries outH errH es = do
  case head es of
    Left x -> (hPutStrLn errH "first line failed. Stopping") >>
                (writeEntry outH errH $ Left x)
    Right _ -> mapM_ (writeEntry outH errH) es -- Write to either file

-- |Writes a single entry to either file depending on if it is left or
-- |right.
writeEntry :: (Show a) => Handle -> Handle -> Either a Entry -> IO ()
writeEntry outHandle _ (Right entry) = B.hPut outHandle $ encode entry
writeEntry _ errHandle (Left err) = hPutStrLn errHandle $ show err

-- |This regex may not fit your needs but for the log files I use,
-- |this is just perfect.
fileRegex = compileString "^.*/(.*)/(.*)\\.[0-9]{4}-[0-9]{2}-[0-9]{2}\\.gz$"

main = do
  args <- getArgs
  when (length args /= 3) $ error "Usage: apache2data threads list_file_name target"

  let threads = read (args !! 0) :: Integer 
  putStrLn $ "Using " ++ (show threads) ++ " threads."

  putStrLn $ "Reading file list from " ++ (args !! 1) ++ "."
  rawList <- readFile (args !! 1)
  let list = (read rawList) :: [(String,Integer)]
  let numberedList = zip [1..] list

  entries <- mapM_ 
  mapM_ (putStrLn.show) list

  -- convertFile (args !! 0) (args !! 1)
  -- putStrLn "Converted."

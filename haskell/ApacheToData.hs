-- | Convert log files into PhasefulSplitter internal
-- | representation. Writes errors to another file for re-processing
-- | later on.

module Main where

import qualified Data.ByteString.Lazy.Char8 as B
import System.IO
import System.Environment
import Data.Binary
import Control.Monad
import System.IO.Unsafe(unsafeInterleaveIO)
import System.Exit
import Control.Parallel.Strategies
import Control.Exception (evaluate)

import Entry
import qualified LineInfo as L
import RegexHelpers
import ApacheLogReader
import ExternalGzip

-- |Writes a single entry to either file depending on if it is left or
-- |right.
writeEntry :: Handle -> Handle -> Either String B.ByteString -> IO ()
writeEntry outHandle _ (Right entry) = B.hPut outHandle entry
writeEntry _ errHandle (Left err) = hPutStrLn errHandle err

-- |Postprocess entry for writing. Left becomes String and Right becomes a binary blob
postprocess (Left a) = Left $ show a
postprocess (Right a) = Right $ encode a

-- |Main.
main = do
  args <- getArgs
  when (length args /= 3) $ error "Usage: apache2data threads list_file_name target"

  let threads = read (args !! 0) :: Int
  putStrLn $ "Using " ++ (show threads) ++ " threads."

  putStrLn $ "Reading file list from " ++ (args !! 1) ++ "."
  rawList <- readFile (args !! 1)
  let instructions = (read rawList) :: (String,[(L.LineInfo,String)])
  let site = fst instructions
  let list = snd instructions

  -- Reads entries from many files and concatenate everything into a
  -- single list.
  entryBlobs <- liftM concat $ mapM verboseRead list
  let entries = map (postprocess.getEitheredEntry) entryBlobs

  -- Prepare "sinks"
  let target = (args !! 2)
  
  outExternalH <- openGzipOutFile (target ++ ".pf.gz")
  let outH = getWriteH outExternalH
  
  errExternalH <- openGzipOutFile (target ++ ".errors.gz")
  let errH = getWriteH errExternalH

  -- Writing to files.
  mapM_ (writeEntry outH errH) entries

  -- Closing and saying goodbyes.
  retOut <- closeExternalHandle outExternalH
  retErr <- closeExternalHandle errExternalH

  when (any (/= ExitSuccess) [retOut,retErr]) $
      error "Writing to gzip files has failed."
  putStrLn "Converted all files."


-- |Read a file into entries and print some progress information. We
-- |are doing it lazily because the processing order and the time of
-- |processing are trivially not meaningful. They are just static log
-- |files.
verboseRead infoPair = unsafeInterleaveIO $ do
  putStrLn $ "Processing file \"" ++ snd infoPair ++ "\""
  blobs <- readBlobsFromFile infoPair
  evaluate blobs -- Suggested by aleator.

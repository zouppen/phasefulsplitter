-- |This module reads PhasefulSplitter Entry format. That is binary
-- |entries gzipped.

module PhasefulReader where

import Entry
import Data.Binary
import Data.Binary.Get
import Control.Monad (liftM)
import Codec.Compression.GZip
import qualified Data.ByteString.Lazy.Char8 as B
import Control.Parallel
import Control.Parallel.Strategies

-- Own datatype is introduced because we want to do "infinite"
-- deserialisation from file to a unknown-length list. List's Binary
-- instance is not sufficient because it puts length of the list in
-- the beginning of data. When serialized lazily, the list length is
-- unknown in the very beginning of file.
data EntryList = EntryList [Entry]

instance Binary EntryList where
     put x = error $ 
             "This type is only a workaroud and shouldn't be " ++
             "serialised. Serialise Entry instead."
                    
     get = liftM EntryList getUntilEmpty
     
-- |A helper function for getting Entries recursively.
getUntilEmpty :: Get [Entry]
getUntilEmpty = do
  stop <- isEmpty
  if stop
     then return []
     else do
       entry <- get
       liftM (entry :) getUntilEmpty

-- |Reads entries from serialized form. Consumes all data available.
readSerialFile :: FilePath -> IO [Entry]
readSerialFile f = do
  fileData <- B.readFile f
  return $ dropWrapper $ decode $ decompress fileData
    where dropWrapper (EntryList e) = e

-- |The ugly and fat processor which tries to be as optimal as
-- |possible. Takes a function, which transforms Entry to a, then a
-- |function which folds a's to b (combining a single file) and then a
-- |function which combines all the results together.
processEverything :: ([[Entry]] -> b) -> [FilePath] -> IO b
processEverything mapReduceF fs = do
  entries <- mapM readSerialFile fs
  return $ mapReduceF entries

-- |Helper function for reducing data from multiple files etc.
--Inspiried by the book "Real World Haskell".
mapReduce
    :: Strategy b  -- For mapping.
    -> (a -> b)    -- Mapping function.
    -> Strategy c  -- For reduction.
    -> ([b] -> c)  -- Reduce function.
    -> [a]         -- List to process.
    -> c
mapReduce mapStg mapF reduceStg reduceF input =
    mapResult `pseq` reduceResult
    where mapResult    = parMap mapStg mapF input
          reduceResult = reduceF mapResult `using` reduceStg

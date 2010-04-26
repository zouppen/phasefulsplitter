-- |This module reads PhasefulSplitter Entry format. That is binary
-- |entries gzipped.

module PhasefulReader where

import Entry
import Data.Binary
import Data.Binary.Get
import Control.Monad (liftM)
import Codec.Compression.GZip
import qualified Data.ByteString.Lazy.Char8 as B

-- Own datatype is introduced because we want to do "infinite"
-- deserialisation from file to a unknown-length list. List's Binary
-- instance is not sufficient because it puts length of the list in
-- the beginning of data. When serialized lazily, the list length is
-- unknown in the very beginning of file.
data EntryList = EntryList [Entry]

instance Binary EntryList where
     put x     = error $ 
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
readSerialEntries :: FilePath -> IO [Entry]
readSerialEntries f = do
  fileData <- B.readFile f
  return $ dropWrapper $ decode $ decompress fileData
    where dropWrapper (EntryList e) = e


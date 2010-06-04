module ParameterToVector where

import System.IO
import Data.List
import Data.List.Split
import Data.Binary
import Control.DeepSeq
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Entry as E
import qualified Data.ByteString.Lazy.Char8 as B
import PhasefulReader
import Ngram

type ResourceGramMap = M.Map String (M.Map String [Ngram Char])
--type ResourceMap = M.Map String Int

-- Using n-sized chunks balance between open file limit and memory
-- constraints.
entryChunkSize = 1000

-- Reads a file, splits it by resources and and writes to files.
processFile :: FilePath -> FilePath -> FilePath -> IO ()
processFile gramFile prefix fromFile = do
  -- Getting resource names and some extra info.
  gramMap <- decodeFile gramFile
  -- Writes a resource numbering scheme to a file.
  writeResources (prefix++"resources.txt") $ resourceToIxMap gramMap
  -- Gets resources from file as nice chunks.
  entryChunks <- chunkedFile fromFile
  -- Processes chunks in a sequence.
  mapM_ (processChunk (entryToResourceIx gramMap) prefix) entryChunks

-- |Splits a file into a number of same-length lists.
chunkedFile :: FilePath -> IO [[E.Entry]]
chunkedFile file = do
  es <- readSerialFile file
  return $ splitEvery entryChunkSize es

writeResources file res = writeFile file $ show $ M.toList res

-- |Processes one chunk into files.
processChunk :: (E.Entry -> Integer) -> String -> [E.Entry] -> IO ()
processChunk entryIx prefix es = do
  return () --TODO

-- |Efficient inserts and element into list which is inside a value of
-- |a Map. Contains no hazardous substances of 'concat'.
updateListMap' :: (Ord a) => M.Map a [b] -> (a,b) -> M.Map a [b]
updateListMap' m (k,v) = m `seq` (k,v) `seq` M.insert k (v:vs) m
    where vs = M.findWithDefault [] k m

-- |Returns a function which converts Entry to the index number of its resource.
entryToResourceIx :: ResourceGramMap -> E.Entry -> Integer
entryToResourceIx m = entryToI
  where resMap = resourceToIxMap m
        entryToI e = (M.!) resMap $ E.exportURLWithoutParams $ E.url e
        
-- |Converts a resource map to index map. Used in giving distinct and
-- |easy-to-type names for files.
resourceToIxMap :: ResourceGramMap -> M.Map String Integer
resourceToIxMap m = M.fromDistinctAscList $ zip (M.keys m) [1..]


-- TRASH BELOW THIS LINE --

{-

groupChunk :: (E.Entry->Int) -> [E.Entry] -> M.Map Int [E.Entry]
groupChunk entryToI es = combineListToMap $ map mapper es
    where mapper x = (entryToI x,x)

-- |Groups a list to a map where fst is the key.
combineListToMap :: (Ord a, NFData a, NFData b) => 
                    [(a,b)] -> M.Map a [b]
combineListToMap xs = foldl' updateListMap M.empty xs


-- processChunk :: (E.Entry->Int) -> FilePath -> [E.Entry] -> IO ()
-- processChunk entryToI prefix es = writeChunk prefix groups
-- where groups = groupChunk entryToI es

-- |Writes the given chunk to files. A chunk contains multiple resources.
--writeChunk :: Binary b => FilePath -> M.Map a [b] -> IO ()
--writeChunk prefix m = mapM_ (writeSingle prefix) list
--    where list = zip [1..] (M.elems m) --FIXME!

-- |Writes one resource data to output file.
--writeSingle :: Binary a => FilePath -> (Int, [a]) -> IO ()
--writeSingle prefix (i,as) = do
--  h <- openFile (prefix ++ show i) AppendMode
--  mapM_ ((B.hPut h) . encode) as
--  hClose h

-- |Filter which keeps all those "interesting" resources. Not tested.
filterInteresting :: [String] -> [E.Entry] -> [E.Entry]
filterInteresting rs es = set `deepseq` myFilter
    where set = S.fromList rs
          myFilter = filter (test . E.exportURLWithoutParams . E.url) es
          test x = S.member x set

-}

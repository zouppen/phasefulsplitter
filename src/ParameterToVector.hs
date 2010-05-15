module ParameterToVector where

import System.IO
import Data.List.Split
import PhasefulReader
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Entry as E

processFile :: FilePath -> FilePath -> IO ()
processFile siteInfoF prefix = do
  siteInfo <- decodeFile siteInfoF
  let res = resourceMap siteInfo
  
resourceMap :: M.Map String (M.Map String [Ngram Char])
            -> M.Map String Int
resourceMap m = M.fromDistinctAscList $ zip (keys m) [1..]
  
processInfile :: FilePath -> FilePath -> (Entry -> (a,b)) -> IO ()
processInfile file prefix f = do
  es <- readSerialFile file
  let chunks = map combineListToMap $ splitEvery entryChunkSize $ map f es
  mapM_ (writeChunk prefix) chunks
  
-- |Writes the given chunk to a files. A chunk contains multiple resources.
writeChunk :: FilePath -> Map a [b] -> IO ()
writeChunk prefix m = mapM_ (writeSingle prefix) list
    where list = zipWith [1..] (elems m)

-- |Writes one resource data to output file.
writeSingle :: FilePath -> (Int, [a]) -> IO ()
writeSingle prefix (i,as) = do
  h <- openFile (prefix ++ show i)
  mapM_ ((B.hPut h) . encode) as
  closeFile h

-- |Groups a list to a map where fst is the key.
combineListToMap :: [(a,b)] -> Map a [b]
combineListToMap xs = foldl' updateListMap M.empty xs

-- |Inserts element into list efficently (with no concat)
updateListMap :: Map a [b] -> (a,b) -> Map a [b]
updateListMap m (k,v) = m `deepseq` (k,v) `deepseq` M.insert k (v:vs) m
    where vs = M.findWithDefault [] k m

-- Using n-sized chunks balance between open file limit and memory
-- constraints.
entryChunkSize = 1000

-- |Filter which keeps all those "interesting" resources. Not tested.
filterInteresting :: [String] -> Entry -> Bool
filterInteresting rs es = set `deepseq` myFilter
    where set = S.fromList rs
          myFilter = filter (test . exportURLWithoutParams . E.url) es
          test x = S.member x set

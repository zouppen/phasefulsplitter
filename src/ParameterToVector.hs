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

processFile :: FilePath -> FilePath -> IO ()
processFile siteInfoF prefix = do
  siteInfo <- decodeFile siteInfoF
  let res = resourceMap siteInfo
  writeResources (prefix++"resources.txt") res

writeResources file res = writeFile file $ show $ M.toList res

resourceMap :: M.Map String (M.Map String [Ngram Char])
            -> M.Map String Int
resourceMap m = M.fromDistinctAscList $ zip (M.keys m) [1..]

processInfile :: (Ord a, NFData a, NFData b, Binary b) =>
                 FilePath -> FilePath -> (E.Entry -> (a,b)) -> IO ()
processInfile file prefix f = do
  es <- readSerialFile file
  let chunks = map combineListToMap $ splitEvery entryChunkSize $ map f es
  mapM_ (writeChunk prefix) chunks
  
-- |Writes the given chunk to a files. A chunk contains multiple resources.
writeChunk :: Binary b => FilePath -> M.Map a [b] -> IO ()
writeChunk prefix m = mapM_ (writeSingle prefix) list
    where list = zip [1..] (M.elems m)

-- |Writes one resource data to output file.
writeSingle :: Binary a => FilePath -> (Int, [a]) -> IO ()
writeSingle prefix (i,as) = do
  h <- openFile (prefix ++ show i) AppendMode
  mapM_ ((B.hPut h) . encode) as
  hClose h

-- |Groups a list to a map where fst is the key.
combineListToMap :: (Ord a, NFData a, NFData b) => 
                    [(a,b)] -> M.Map a [b]
combineListToMap xs = foldl' updateListMap M.empty xs

-- |Inserts element into list efficently (with no concat)
updateListMap :: (Ord a, NFData a, NFData b) =>
                 M.Map a [b] -> (a,b) -> M.Map a [b]
updateListMap m (k,v) = m `deepseq` (k,v) `deepseq` M.insert k (v:vs) m
    where vs = M.findWithDefault [] k m

-- Using n-sized chunks balance between open file limit and memory
-- constraints.
entryChunkSize = 1000

-- |Filter which keeps all those "interesting" resources. Not tested.
filterInteresting :: [String] -> [E.Entry] -> [E.Entry]
filterInteresting rs es = set `deepseq` myFilter
    where set = S.fromList rs
          myFilter = filter (test . E.exportURLWithoutParams . E.url) es
          test x = S.member x set

module Main where

import System.IO
import Data.List
import Data.List.Split
import Data.Binary
--import Control.DeepSeq
import Control.Monad
import System.Environment
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Entry as E
import qualified Data.ByteString.Lazy.Char8 as B
import PhasefulReader
import Ngram
import LineInfo
import URLNgram
import Helpers (findWithErrorF,toIxMap)

type ResourceGramMap = M.Map String (M.Map String [Ngram Char])
type EntryToIx = E.Entry -> Int

-- Using n-sized chunks balance between open file limit and memory
-- constraints.
entryChunkSize = 1000

main = do
  args <- getArgs
  when (length args /= 3) $ error "Usage: parameter2vector gram_file input_file out_prefix"
  processFile (args !! 0) (args !! 2) (args !! 1)
  putStrLn "Done!"

-- |Reads a file, splits it by resources and and writes to files.
processFile :: FilePath -> FilePath -> FilePath -> IO ()
processFile gramFile prefix fromFile = do
  -- Getting resource names and some extra info.
  gramMap <- decodeFile gramFile
  -- Writes a resource numbering scheme to a file.
  writeResources (prefix++"resources.txt") $ toIxMap gramMap
  -- Gets resources from file as nice chunks.
  entryChunks <- chunkedFile fromFile
  -- Processes chunks in a sequence.
  mapM_ (processChunk (entryToResourceIx gramMap) (entryToText gramMap) prefix) entryChunks

-- |Splits a file into a number of same-length lists.
chunkedFile :: FilePath -> IO [[E.Entry]]
chunkedFile file = do
  es <- readSerialFile file
  return $ splitEvery entryChunkSize es

writeResources file res = writeFile file $ show $ M.toList res

-- |Processes one chunk into files.
processChunk :: EntryToIx -> (EntryToIx -> E.Entry -> String) -> String -> [E.Entry] -> IO ()
processChunk entryIx converter prefix es = mapM_ writeAll $ M.toList entryMap
    where entryMap = groupChunk entryIx es
          writeAll (k,vs) = appendFile (prefix++show k) $ encodeAll vs
          encodeAll vs = unlines $ map (converter entryIx) vs

-- |Efficiently inserts and element into list which is inside a value
-- of a Map. Contains no hazardous substances of 'concat'.
updateListMap' :: (Ord a) => M.Map a [b] -> (a,b) -> M.Map a [b]
updateListMap' m (k,v) = m `seq` (k,v) `seq` M.insert k (v:vs) m
    where vs = M.findWithDefault [] k m

-- |Returns a function which converts Entry to the index number of its resource.
entryToResourceIx :: ResourceGramMap -> EntryToIx
entryToResourceIx m e = (M.!) resMap $ E.exportURLWithoutParams $ E.url e
  where resMap = toIxMap m

groupChunk :: EntryToIx -> [E.Entry] -> M.Map Int [E.Entry]
groupChunk entryToI es = combineListToMap $ map mapper es
    where mapper x = (entryToI x,x)
          combineListToMap xs = foldl' updateListMap' M.empty xs

data GramOut = GramOut {
      info :: LineInfo
    , method :: B.ByteString
    , protocol :: B.ByteString
    , response :: Integer
    , bytesLn2 :: Integer
    , resource :: Int
    , gramVector :: [Integer]
    }

-- |Converts the given Entry to text.
entryToText resMap eToIx = vectorToText.(entryToVector resMap eToIx)

-- |Converts an Entry to GramOut data structure.
entryToVector :: ResourceGramMap -> EntryToIx -> E.Entry -> GramOut
entryToVector gramMap eToIx e =
  GramOut (E.info e) (E.method e) (E.protocol e) (E.response e) bytesLn res grams
        where bytesLn = fancyMagnitude $ E.bytes e
              resPair = toResourcePair e              
              res = eToIx e
              grams = resourceVector ((M.!) gramMap (fst resPair)) (snd resPair)

-- |Converts GramOut to a nice "string vector".
vectorToText :: GramOut -> String
vectorToText (GramOut (LineInfo a b c) method protocol response bytesLn2 resource gramVector) =
    intercalate "," $ show a:show b:show c:show methodIx:show protoIx:
                show response:show bytesLn2:show resource:map show gramVector
                  where protoIx = mapToNumber httpProtocols protocol
                        methodIx = mapToNumber httpMethods method
                     
resourceVector :: (M.Map String [Ngram Char]) -> ResourceStat -> [Integer]
resourceVector gramMap (ResourceStat _ paramMap) =
    concat $ map (paramVector paramMap) (M.toList gramMap)

paramVector :: (M.Map String ParamInfo) -> (String,[Ngram Char]) -> [Integer]
paramVector curParams (param,grams) = nGramToVector grams curGrams
    where curGrams = case M.lookup param curParams of
                       Nothing -> M.empty
                       Just (ParamInfo _ x) -> x

httpMethods = map B.pack ["HEAD","GET","POST","PUT","DELETE","TRACE","OPTIONS",
                          "CONNECT","PATCH"]

httpProtocols = map B.pack ["HTTP/1.0","HTTP/1.1"]

-- |Maps name to a number. Usual type for k is String.
mapToNumber :: (Show k, Ord k, Integral a) => [k] -> k -> a
mapToNumber list = findWithErrorF msg $ M.fromList $ zip list [1..] 
  where msg x = "HTTP-koodia "++show x++" ei löytynyt."

-- |Calculates "fancy" magnitude of a number. That's modified base 2 logarithm.
fancyMagnitude 0 = 0
fancyMagnitude x = 1 + (floor $ logBase 2 $ fromInteger x)
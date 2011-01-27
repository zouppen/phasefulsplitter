-- |Uses PhasefulReader to process through given files.

module Main where

import Data.Binary
import qualified Data.Map as M
import Control.Monad (liftM2)
import System.Environment
import Network.URL
import Control.Parallel.Strategies
import Control.DeepSeq
import Data.List
import PhasefulReader
import Entry
import Ngram
import URLNgram

main = do
  files <- getArgs
  result <- processEverything reduceF files
  writeFile "ngrams_raw.txt" $ show result
  encodeFile "ngrams.out" $ flatMap result
  putStrLn "Processing ready."

-- |Combines two ParamInfos (sums n-gram frequencies etc).
paramSum :: ParamInfo -> ParamInfo -> ParamInfo
paramSum (ParamInfo count_1 grams_1) (ParamInfo count_2 grams_2) = 
    ParamInfo (count_1+count_2) (M.unionWith (+) grams_1 grams_2)

-- |Combines two ResourceStats into one (sums frequencies etc).
resourceSum :: ResourceStat -> ResourceStat -> ResourceStat
resourceSum (ResourceStat count_1 params_1) (ResourceStat count_2 params_2) =
    ResourceStat (count_1+count_2) (M.unionWith paramSum params_1 params_2)

insertToPool :: M.Map String ResourceStat -> Entry -> M.Map String ResourceStat
insertToPool resmap entry = resmap `deepseq` res `deepseq` M.insertWith resourceSum (fst res) (snd res) resmap
    where res = toResourcePair entry

combineF :: [M.Map String ResourceStat] -> M.Map String ResourceStat
combineF = M.unionsWith resourceSum

mappingF :: [Entry] -> M.Map String ResourceStat
mappingF = foldl' insertToPool M.empty

reduceF = mapReduce rseq mappingF rseq combineF

flatMap :: M.Map String ResourceStat 
        -> M.Map String (M.Map String [Ngram Char])
flatMap = M.map flatRs

flatPi :: ParamInfo -> [Ngram Char]
flatPi (ParamInfo _ grams) = M.keys grams

flatRs :: ResourceStat -> M.Map String [Ngram Char] 
flatRs (ResourceStat _ params) = M.map flatPi params

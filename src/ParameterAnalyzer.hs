-- |Uses PhasefulReader to process through given files.

module Main where

import qualified Data.Map as M
import System.Environment
import Network.URL
import Control.Parallel.Strategies
import Data.List
import PhasefulReader
import Entry
import Ngram

data ParamInfo = ParamInfo {
      paramCount :: Integer                    -- ^Frequency of this parameter.
    , ngramMap   :: M.Map (Ngram Char) Integer -- ^N-grams.
    } deriving (Show,Eq)

data ResourceStat = ResourceStat {
      resourceCount :: Integer                 -- ^Frequency of this resource.
    , params        :: M.Map String ParamInfo  -- ^Key and its n-gram.
} deriving (Show,Eq)

instance NFData ResourceStat where
    rnf (ResourceStat x y) = rnf x `seq` rnf y

instance NFData ParamInfo where
    rnf (ParamInfo x y) = rnf x `seq` rnf y

main = do
  files <- getArgs
  result <- processEverything reduceF files
  putStrLn $ show result

-- |Builds "initial" frequency table one resource and its parameters.
toResourcePair :: Entry -> (String,ResourceStat)
toResourcePair e = (key,value)
    where params = url_params $ url e
          key    = exportURLWithoutParams $ url e
          value  = ResourceStat 1 $ getAllGrams params

-- |Forms parameter map for ResourceStat.
getAllGrams :: [(String, String)] -> M.Map String ParamInfo
getAllGrams params = M.fromList $ map toGram params
    where toGram (k,v) = (k,ParamInfo 1 $ frequencyNgram 2 v)

-- |Combines two ParamInfos (sums n-gram frequencies etc).
paramSum :: ParamInfo -> ParamInfo -> ParamInfo
paramSum (ParamInfo count_1 grams_1) (ParamInfo count_2 grams_2) = 
    ParamInfo (count_1+count_2) (M.unionWith (+) grams_1 grams_2)

-- |Combines two ResourceStats into one (sums frequencies etc).
resourceSum :: ResourceStat -> ResourceStat -> ResourceStat
resourceSum (ResourceStat count_1 params_1) (ResourceStat count_2 params_2) =
    ResourceStat (count_1+count_2) (M.unionWith paramSum params_1 params_2)

insertToPool :: M.Map String ResourceStat -> Entry -> M.Map String ResourceStat
insertToPool resmap entry = M.insertWith' resourceSum (fst res) (snd res) resmap
    where res = toResourcePair entry

combineF :: [M.Map String ResourceStat] -> M.Map String ResourceStat
combineF = M.unionsWith resourceSum

mappingF :: [Entry] -> M.Map String ResourceStat
mappingF = foldl' insertToPool M.empty  

reduceF = mapReduce rdeepseq mappingF rdeepseq combineF

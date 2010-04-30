-- |Uses PhasefulReader to process through given files.

module Main where

import qualified Data.Map as M
import System.Environment
import Network.URL
import PhasefulReader
import Entry
import Control.Parallel.Strategies

data ResourceStat = ResourceStat {
      count  :: Integer            -- Frequency of this resource in the data set
    , params :: M.Map String Integer -- Key and its number of occurrences
} deriving (Show,Eq)

instance NFData ResourceStat where
  rnf (ResourceStat x y) = rnf x `seq` rnf y

main = do
  files <- getArgs
  result <- processEverything modifier folder combiner files
  putStrLn $ show result
  
-- |Builds "initial" frequency table one resource and its parameters.
modifier :: Entry -> (String,ResourceStat)
modifier e = (exportURLWithoutParams $ url e,ResourceStat 1 paramMap)
    where paramNames = map fst $ url_params $ url e
          paramMap = M.fromList $ zip paramNames [1..]

folder = M.fromListWith reduceTwo
combiner = M.unionsWith reduceTwo

reduceTwo :: ResourceStat -> ResourceStat -> ResourceStat
reduceTwo (ResourceStat c_1 p_1) (ResourceStat c_2 p_2) =
    ResourceStat (c_1+c_2) (M.unionWith (+) p_1 p_2)

-- |Used in choosing interesting resources from large amounts of
-- |resources.

module InterestingParameters where

import Data.List
import qualified Data.Map as M
import URLNgram
import Helpers (toIxMap)

readGramInfo :: FilePath -> IO (M.Map String ResourceStat)
readGramInfo f = do a <- readFile f
                    return $ read a

resIntensityVector :: ResourceStat -> [Integer]
resIntensityVector (ResourceStat _ params) = concat $ map parIntensityVector $ M.elems params

parIntensityVector :: ParamInfo -> [Integer]
parIntensityVector (ParamInfo _ gramMap) = M.elems gramMap

intVariance :: (Fractional a) => [Integer] -> a
intVariance xs = sum $ map deviation xs
    where deviation x = ((fromInteger x) - (intAvg xs)) ^ 2

intAvg :: (Fractional a) => [Integer] -> a
intAvg xs = fromInteger (sum xs) / (genericLength xs)

data Interesting = Interesting {
      count      :: Integer
    , paramCount :: Int
    , stdDev     :: Float
    , grams      :: Int
    } deriving (Show)

interesting :: ResourceStat -> Interesting
interesting res =
    Interesting (resourceCount res) (M.size $ params res) (intVariance vector) (length vector)
    where vector = resIntensityVector res
          
filterAll x = and [filterCount x,filterGrams x]
filterCount x = 99 < (count x)
filterGrams x = 9 < (grams x)

getRelevant m = M.filter filterAll $ M.map interesting m

printRelevant m = putStr $ unlines $ map resStr $ M.keys $ getRelevant m
    where resStr k = (show $ ix k) ++ " " ++ k
          ix x = (M.!) (toIxMap m) x

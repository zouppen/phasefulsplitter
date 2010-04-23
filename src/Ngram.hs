-- |Algorithms for generating and processing of n-grams. Based on the
-- |ideas of Gil Davis in his PhD thesis. "Anomaly Detection and
-- |Classification via Diffusion Processes in Hyper-Networks.

module Ngram where

import qualified Data.Map as M

data Location = Begin | In | End  deriving (Show,Ord,Eq)
data Ngram a = Ngram Location [a]  deriving (Show,Ord,Eq)

-- |Transforms a list into list of slining window n-grams. Wraps
-- |individual n-grams inside Ngram which takes care of Begin and End
-- |tokens.
nGrams :: Int -> [a] -> [Ngram a]
nGrams n xs = (Ngram Begin [head xs]):(Ngram End [last xs]):(map (Ngram In) $ rawNgram n xs)

-- |Produces n-grams in /raw/ form. That is an n-gram without Begin
-- |and End tokens.
rawNgram :: Int -> [a] -> [[a]]
rawNgram n xs = take (length xs-n+1) $ rawNgram' n xs
rawNgram' n xs = (take n xs):(rawNgram' n $ tail xs)

-- |Produces frequency map from n-grams (or any other instance of Eq)
frequencyMap :: (Ord a) => [a] -> M.Map a Integer
frequencyMap xs = M.fromListWith (+) $ map (\x -> (x,1)) xs

-- |Forms frequency table of n-grams from a single list.
frequencyNgram :: (Ord a) => Int -> [a] -> M.Map (Ngram a) Integer
frequencyNgram n xs = frequencyMap $ nGrams n xs

-- |Forms frequency table of n-grams from many lists. Takes care of
-- |Begins and Ends properly (every list has its own Begin and End
-- |tokens.
giganticFrequencyNgram :: (Ord a) => Int -> [[a]] -> M.Map (Ngram a) Integer
giganticFrequencyNgram n xss = frequencyMap $ concat $ map (nGrams n) xss

-- |Returns a vectorized (=list) form of n-gram. The second parameter
-- is zero map containing all possible n-grams as keys. Zero map can
-- be generated from 'giganticFrequencyNgram' result with
-- 'toZeroMap'. Vectorized form can be used as output to dimension
-- reduction algorithms.
nGramVector :: (Ord k) => [k] -> M.Map k Integer -> [Integer]
nGramVector keyList x = map (getGramCount x) keyList

-- |Returns zero if a key is not found in that map, otherwise returns
-- |the value.
getGramCount :: (Ord k) => M.Map k Integer -> k -> Integer
getGramCount gramMap gram = M.findWithDefault 0 gram gramMap

-- |Algorithms for generating and processing of n-grams. Based on the
-- |ideas of Gil Davis in his PhD thesis. "Anomaly Detection and
-- |Classification via Diffusion Processes in Hyper-Networks.

module Ngram where

import Data.Binary
import qualified Data.Map as M
import Control.Parallel.Strategies
import Control.Monad (liftM2)

data Location = BeginEnd | Begin | In | End  deriving (Show,Read,Ord,Eq)
data Ngram a = Ngram Location [a]  deriving (Show,Read,Ord,Eq)

instance (NFData a) => NFData (Ngram a) where
    rnf (Ngram loc xs) = loc `seq` rnf xs

instance (Binary a) => Binary (Ngram a) where
    put (Ngram a b) = put a >> put b
    get = liftM2 Ngram get get
    
instance Binary Location where
    put BeginEnd = putWord8 0
    put Begin    = putWord8 1
    put In       = putWord8 2
    put End      = putWord8 3
    get = do 
      t <- getWord8
      return $ case t of
                 0 -> BeginEnd
                 1 -> Begin
                 2 -> In
                 3 -> End

-- |Transforms a list into list of sliding window n-grams. Wraps
-- |individual n-grams inside Ngram which takes care of Begin and End
-- |tokens.
nGrams :: Int -> [a] -> [Ngram a]
nGrams _ [] = [Ngram BeginEnd []] -- Special gram for empty string.
nGrams n xs = (Ngram Begin (take (n-1) xs)):
              (Ngram End (takeEnd (n-1) xs)):
              (map (Ngram In) $ rawNgram n xs)

-- |Takes a substring from end of given string.
takeEnd n s = drop (length s - n) s

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

-- |Returns a vectorized (=list) form of n-gram. The first parameter
-- |is list of all possible (or interesting n-grams) and the second
-- |parameter is gram map of current n-grams. Function returns a list
-- |(vector) of frequencies.
nGramToVector :: (Ord k) => [k] -> M.Map k Integer -> [Integer]
nGramToVector keyList x = map (getGramCount x) keyList

-- |Returns zero if a key is not found in that map, otherwise returns
-- |the value.
getGramCount :: (Ord k) => M.Map k Integer -> k -> Integer
getGramCount gramMap gram = M.findWithDefault 0 gram gramMap

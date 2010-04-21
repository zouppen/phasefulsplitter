-- |Algorithms for generating and processing of n-grams. Based on the
-- |ideas of Gil Davis in his PhD thesis. "Anomaly Detection and
-- |Classification via Diffusion Processes in Hyper-Networks.

module Ngram where

data GramElement a = Begin | El a | End  deriving (Show,Eq)

-- |Transforms a list into gram list suitable for grammifying. Wraps
-- |list inside GramElement so Begin and End is taken care.
toGramList xs = Begin : toGramList' xs
toGramList' [] = [End]
toGramList' (x:xs) = (El x):(toGramList' xs)

-- |Produces n-grams in /raw/ form. That is, begin and end is not
-- |there.
grammify n xs = take (length xs-n+1) $ grammify' n xs
grammify' n xs = (take n xs):(grammify' n $ tail xs)

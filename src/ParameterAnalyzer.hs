-- |Uses PhasefulReader to process through given files.

module Main where

import Data.Map
import System.Environment
import Network.URL
import PhasefulReader
import Entry


main = do
  files <- getArgs
  result <- processEverything modifier folder combiner files
  putStrLn $ show result
  
modifier :: Entry -> (String,Integer)
modifier e = (exportURLWithoutParams $ url e,1)
folder = fromListWith (+)
combiner = unionsWith (+)

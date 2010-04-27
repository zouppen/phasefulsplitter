-- |Uses PhasefulReader to process through given files.

module ParameterAnalyzer where

import PhasefulReader

main = do
  files <- getArgs
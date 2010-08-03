module Main where

import System.Environment (getArgs)
import Control.Monad (when)
import RandomHelpers (pick)
import System.Random (newStdGen)
import qualified Data.ByteString.Lazy.Char8 as B

-- |Picks given number of lines randomly from a text file, writing to
-- a new file.
pickLines :: Int -> FilePath -> FilePath -> IO ()
pickLines n fromFile toFile = do
  gen <- newStdGen
  file <- B.readFile fromFile
  let picked = pick n (B.lines file) gen
  B.writeFile toFile $ B.unlines picked

main = do
  args <- getArgs
  when (length args /= 3) $ error "Usage: linepicker maximum input_file output_file"
  let n = read (args !! 0) :: Int
  
  pickLines n (args !! 1) (args !! 2)


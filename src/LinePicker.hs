module LinePicker where

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

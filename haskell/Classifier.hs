-- |Uses IxonosFileListClassifier to classify given file list of
-- |files. In future this file may support other logging structures.

module Main where

import System.Environment
import qualified Data.ByteString.Lazy.Char8 as B
import IxonosFileListClassifier

main = do
  args <- getArgs
  prog <- getProgName
  case length args of
    1 -> do fileList <- B.getContents
            extractFileData fileList (args !! 0)
    2 -> do fileList <- B.readFile (args !! 0)
            extractFileData fileList (args !! 1)
    _ -> do putStrLn $ "Usage 1: find args... | "++prog++" basename"
            putStrLn $ "Usage 2: "++prog++" path_to_file_list basename"
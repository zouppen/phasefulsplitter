-- | Produces a file list by parsing the log file naming conventions
-- | used at Ixonos. You can use this module as a example when
-- | creating your own classifier.

module IxonosFileListClassifier where

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map as M
import RegexHelpers

-- |This regex may not fit your needs but for the log files I use,
-- |this is just perfect.
fileRegex = compileString "^.*/(.*)/(.*)\\.[0-9]{4}-[0-9]{2}-[0-9]{2}\\.gz$"

extractFileData filePath baseName = do
  mapServer <- readServerFile
  file <- B.readFile filePath
  let sites = listOfSites $ map (extractParams mapServer) $ B.lines file
  mapM_ (writeSite baseName) sites

-- |Writes site information to a file.
writeSite :: (Show t) => FilePath -> (String, t) -> IO ()
writeSite baseName (site,x) = writeFile (baseName++site) $ show x

listOfSites files = M.toList $ M.fromListWith (++) files

-- |Extracts values from a string in for compatible with
-- |Map.fromListWith. Format is (site_name,(filepath,server_id)).
extractParams :: (String -> Integer) -> B.ByteString -> (String, [(String, Integer)])
extractParams mapServer path = case splitBS fileRegex path of
                    Nothing -> error $ "Parse error in filename: " ++ B.unpack path
                    Just [server,site] -> (B.unpack site,[(B.unpack path,mapServer $ B.unpack server)])

-- |Reads server name-id-mappings from server_map.txt.
readServerFile :: IO (String -> Integer)
readServerFile = do
  contents <- readFile "server_map.txt"
  return $ (M.!) $ M.fromList $ (read contents::[(String,Integer)])
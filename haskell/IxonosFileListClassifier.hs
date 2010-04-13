-- | Produces a file list by parsing the log file naming conventions
-- | used at Ixonos. You can use this module as a example when
-- | creating your own classifier.

module IxonosFileListClassifier where

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map as M
import RegexHelpers
import LineInfo
import Helpers

-- |This regex may not fit your needs but for the log files I use,
-- |this is just perfect.
fileRegex = compileString "^.*/(.*)/(.*)\\.[0-9]{4}-[0-9]{2}-[0-9]{2}\\.gz$"

-- |Produces file lists for further processing
extractFileData fileList baseName = do
  mapServer <- readServerFile
  let sites = listOfSites $ map (extractParams mapServer) $ B.lines fileList
  mapM_ (writeSite baseName) sites

-- |Writes site information to a file.
writeSite :: String -> (String, [(FilePath, Integer)]) -> IO ()
writeSite baseName (site,x) = writeFile (baseName++site) $
                              showNiceList fileInfo ""
    where fileInfo = zipWith formLineInfo [1..] x

-- |Forms fileInfo from "raw" data.
formLineInfo :: Integer -> (String, Integer) -> (LineInfo,String)
formLineInfo i (filePath,serverId) = (LineInfo i serverId 0,filePath)

-- |Used to group files from one site into each element
-- |in the resulting list.
listOfSites files = M.toList $ M.fromListWith (++) files

-- |Extracts values from a path string to a format compatible with
-- |Map.fromListWith. Format is (site_name,[(filepath,server_id)]).
-- |Parameter 'mapServer' is the function for mapping servers to
-- |strings (see 'readServerFile'). The list in snd contains only
-- |one item.
extractParams :: (String -> Integer) -> B.ByteString -> (String, [(String, Integer)])
extractParams mapServer path = case splitBS fileRegex path of
                    Nothing -> error $ "Parse error in filename: " ++ B.unpack path
                    Just [server,site] -> (B.unpack site,[(B.unpack path,mapServer $ B.unpack server)])

-- |Reads server name-id-mappings from server_map.txt.
readServerFile :: IO (String -> Integer)
readServerFile = do
  contents <- readFile "server_map.txt"
  return $ findWithErrorF
             (\k -> "Unknown server name \""++k++"\". Check server_map.txt")
             (M.fromList $ (read contents::[(String,Integer)]))

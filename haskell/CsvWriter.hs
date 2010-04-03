module CsvWriter where

import ApacheLogReader (readEntriesFromFile)
import qualified ApacheParser as A

import qualified Data.ByteString.Lazy.Char8 as B
import Data.Either (rights,lefts)

import Data.Time.Format
import System.Locale

colDesc = B.pack ";timestamp,method,url_len,protocol,response_code,bytes,referer_len,browser_len"

entryToText :: A.Entry -> B.ByteString
entryToText entry = B.intercalate (B.pack ",") [
                     B.pack $ toTimeStamp $ A.date entry,
                     A.method entry,
                     B.pack $ show (A.url entry),
                     A.protocol entry,
                     B.pack $ show (A.response entry),
                     B.pack $ show (A.bytes entry),
                     B.pack $ show (B.length (A.referer entry)),
                     B.pack $ show (B.length (A.browser entry))
                    ]
                    
processFile fromFile toFile errorFile = do
  entries <- readEntriesFromFile fromFile
  B.writeFile toFile $ B.unlines $ (colDesc:) $ map entryToText $ rights entries
  writeFile errorFile $ show $ lefts entries

-- |Outputs timestamp in a not-so-sophisticated way
toTimeStamp t = formatTime defaultTimeLocale "%s" t

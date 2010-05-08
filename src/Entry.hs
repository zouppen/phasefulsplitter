module Entry (Entry(Entry),codecOK,exportURLWithoutParams,url) where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.Time.Clock
import Data.Time.Format
import Network.URL
import Data.Binary
import Control.Monad (liftM,liftM3,ap)
import Control.DeepSeq
import LineInfo
import UTCTimeExts

data Entry = Entry {
      info      :: LineInfo     -- Connects log file lines to this entries.
    , ip        :: B.ByteString
    , date      :: UTCTime
    , method    :: B.ByteString
    , url       :: URL
    , protocol  :: B.ByteString
    , response  :: Integer
    , bytes     :: Integer
    , referer   :: B.ByteString
    , browser   :: B.ByteString
} deriving (Show,Eq)

instance NFData B.ByteString where
    rnf bs = rnf $ B.head bs -- Maybe this is enough to trigger evaluation
    
instance NFData URL where
    rnf (URL a b c) = rnf a `seq` rnf b `seq` rnf c
    
instance NFData URLType where
    rnf (Absolute host) = rnf host
    rnf HostRelative = ()
    rnf PathRelative = ()
    
instance NFData Host where
    rnf (Host a b c) = rnf a `seq` rnf b `seq` rnf c
    
instance NFData Protocol where
    rnf (RawProt x) = rnf x
    rnf (HTTP x) = rnf x
    rnf (FTP x) = rnf x
    
instance NFData Entry where
    rnf (Entry a b c d e f g h i j) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e `seq` rnf f `seq` rnf g `seq` rnf h `seq`rnf i `seq` rnf j
   
-- |Implicit serialisation of URL (handles only host relative URLs)
instance Binary URL where
    put (URL host_t path params) = put host_t >> put path >> put params 
    get = liftM3 URL get get get

instance Binary URLType where
    put (Absolute (Host (HTTP ht) h p)) = putWord8 0 >> put ht >> put h >> put p
    put HostRelative = putWord8 1
    put PathRelative = putWord8 2
    put _ = error "Unsupported URL type"
    get = do 
      t <- getWord8
      case t of
        0 -> do
            ht <- get
            h <- get
            p <- get
            return $ Absolute (Host (HTTP ht) h p)
        1 -> return HostRelative
        2 -> return PathRelative
            
instance Binary Entry where
    put (Entry info ip date method url protocol response bytes referer browser) =
        put info >>
        put ip >> put date >> put method >> put url >> put protocol >>
        put response >> put bytes >> put referer >> put browser
    get = return Entry `ap` get `ap` get `ap` get `ap` get `ap` get
          `ap` get `ap` get `ap` get `ap` get `ap` get

encdec :: (Binary a) => a -> a
encdec = decode . encode

-- |Tests if data stays the same after encoding and decoding
codecOK :: (Binary a, Eq a) => a -> Bool
codecOK x = x == (decode.encode) x

-- |A modified function from Network.URL. This exports just URL without
-- parameters.
exportURLWithoutParams :: URL -> String
exportURLWithoutParams url = absol ++ the_path
  where
  absol       = case url_type url of
                  Absolute hst -> exportHost hst ++ "/"
                  HostRelative  -> "/"
                  PathRelative  -> ""

  the_path    = encString False ok_path (url_path url)


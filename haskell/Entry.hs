module Entry (Entry(Entry),codecOK) where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.Time.Clock
import Data.Time.Calendar (fromGregorian)
import Data.Time.Format
import Network.URL
import Data.Binary
import Control.Monad (liftM,liftM3,ap)

data Entry = Entry {
      server_id :: Integer
    , line_n    :: Integer
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

-- |Implicit serialisation of UTCTime
instance Binary UTCTime where
    put d = put (toUnixSeconds d)
    get = liftM fromUnixSeconds get

-- |Implicit serialisation of URL (handles only host relative URLs} 
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
    put (Entry site line ip date method url protocol response bytes referer browser) =
        put site >> put line >>
        put ip >> put date >> put method >> put url >> put protocol >>
        put response >> put bytes >> put referer >> put browser
    get = return Entry `ap` get `ap` get `ap` get `ap` get `ap` get `ap` get
          `ap` get `ap` get `ap` get `ap` get `ap` get

unixEpoch = UTCTime (fromGregorian 1970 1 1) 0

-- |Converts UTCTime to UNIX time stamp seconds. Please note! This
-- truncates second fractions.
toUnixSeconds :: UTCTime -> Integer
toUnixSeconds d = truncate $ diffUTCTime d unixEpoch

fromUnixSeconds :: Integer -> UTCTime
fromUnixSeconds s = addUTCTime (fromInteger s) unixEpoch

encdec :: (Binary a) => a -> a
encdec = decode . encode

-- !Tests if data stays the same after encoding and decoding
codecOK :: (Binary a, Eq a) => a -> Bool
codecOK x = x == (decode.encode) x
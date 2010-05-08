module LineInfo where

import Control.Monad
import Data.Binary
import Control.DeepSeq

data LineInfo = LineInfo {
      fileId   :: Integer
    , serverId :: Integer
    , lineNo   :: Integer
} deriving (Show,Read,Eq)

instance Binary LineInfo where
    put (LineInfo file_id line server_id) = put file_id >> put line >> put server_id
    get = return LineInfo `ap` get `ap` get `ap` get

instance NFData LineInfo where
    rnf (LineInfo a b c) = rnf a `seq` rnf b `seq` rnf c

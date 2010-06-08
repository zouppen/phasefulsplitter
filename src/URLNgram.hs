module URLNgram where

import Data.Binary
import qualified Data.Map as M
import Control.Parallel.Strategies
import Control.Monad (liftM2)
import Network.URL (url_params)
import Ngram
import Entry

data ParamInfo = ParamInfo {
      paramCount :: Integer                    -- ^Frequency of this parameter.
    , ngramMap   :: M.Map (Ngram Char) Integer -- ^N-grams.
    } deriving (Show,Eq)

data ResourceStat = ResourceStat {
      resourceCount :: Integer                 -- ^Frequency of this resource.
    , params        :: M.Map String ParamInfo  -- ^Key and its n-gram.
} deriving (Show,Eq)

instance NFData ResourceStat where
    rnf (ResourceStat x y) = rnf x `seq` rnf y

instance NFData ParamInfo where
    rnf (ParamInfo x y) = rnf x `seq` rnf y

instance Binary ResourceStat where
    put (ResourceStat a b) = put a >> put b
    get = liftM2 ResourceStat get get
    
instance Binary ParamInfo where
    put (ParamInfo a b) = put a >> put b
    get = liftM2 ParamInfo get get

-- |Builds "initial" frequency table one resource and its parameters.
toResourcePair :: Entry -> (String,ResourceStat)
toResourcePair e = (key,value)
    where params = url_params $ url e
          key    = exportURLWithoutParams $ url e
          value  = ResourceStat 1 $ getAllGrams params

-- |Forms parameter map for ResourceStat.
getAllGrams :: [(String, String)] -> M.Map String ParamInfo
getAllGrams params = M.fromList $ map toGram params
    where toGram (k,v) = (k,ParamInfo 1 $ frequencyNgram 2 v)

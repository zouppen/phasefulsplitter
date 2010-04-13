module LineInfo where

data LineInfo = LineInfo {
      fileId   :: Integer
    , serverId :: Integer
    , lineNo   :: Integer
} deriving (Show,Read,Eq)

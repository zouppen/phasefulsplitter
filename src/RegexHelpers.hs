module RegexHelpers (compileString, splitBS) where

import qualified Data.ByteString.Lazy.Char8 as B
import Text.Regex.TDFA.ByteString.Lazy
import Text.Regex.TDFA.Common

-- |Compiles String into a TDFA Regex with good options.
compileString :: String -> Regex
compileString regexText = fromEither $ compile CompOption {caseSensitive = False, multiline = False, rightAssoc = True, newSyntax = True, lastStarGreedy = False} ExecOption {captureGroups = True} (B.pack regexText)

-- |Splits a given bytestring to a list of groups with a Regex.
splitBS :: Regex -> B.ByteString -> Maybe [B.ByteString]
splitBS regex bs = case match of 
                   Left s -> Nothing
                   Right Nothing -> Nothing
                   Right (Just (_,_,_,ms)) -> Just ms
  where match = regexec regex bs

-- Helpers for helpers:

-- |Transforms "Either errors" to exception errors. Haskell has 8
-- types of errors, so this is only a minor help.
fromEither :: Either String t -> t
fromEither = either error id

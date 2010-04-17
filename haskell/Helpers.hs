-- |Helper functions
module Helpers where

import qualified Data.Map as M
import Data.List (transpose)
import Data.List.Split (splitEvery)

-- |Returns a getter for a map which throws given error message if
-- |element is not found in the map. This is used to get nicer error
-- |messages than "element not in the map" to the user. If you don't
-- |like having error function you can just pass @const "message"@.
findWithErrorF :: (Ord k) => (k -> String) -> M.Map k b -> k -> b
findWithErrorF f map k = M.findWithDefault (error $ f k) k map

-- |Alternative show function. Used to produce nicer list than with
-- |default Show instance. Compatible with default readList.
showNiceList :: (Show t) => [t] -> ShowS
showNiceList []	= showString "[]"
showNiceList (x:xs) = showChar '[' . shows x . showl xs    
    where showl [] = showString "\n]\n"
          showl (x:xs) = showString "\n," . shows x . showl xs

-- |Unmerge splits a list into 'n' sublists. List is evaluated lazily
-- |so every list may be consumed at the same time.

-- Without using (take n) getting length of the stream requires
-- traversal of the whole list. Take doesn't drop anything (the list
-- contains n elements anyway.
unmerge :: Int -> [a] -> [[a]]
unmerge n list = take n $ transpose $ splitEvery n list

-- |Helper functions
module Helpers where

import qualified Data.Map as M

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

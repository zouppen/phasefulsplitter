module RandomHelpers where

import System.Random (randoms,StdGen,Random)

--orderedRandom :: 
--              -> Integer -- ^ Random value 0..1.
--              -> 
randomPosition :: (Integral a, Floating t) =>
                  a -- ^ Number of randomizations left.
               -> t -- ^ Random value 0..1.
               -> t -- ^ "Loaded" random value.
randomPosition n rand = 1 - (1-rand) ** (1/(fromIntegral n))

randomPositions :: (Integral a, Random b, Floating b) => a -> StdGen -> [b]
randomPositions n gen = zipWith randomPosition (stepper n 1) (randoms gen)

-- |Returns a list stepping every integer between from and to,
-- inclusive. This is different to [x..y] in the sense it can produce
-- decreasing lists, too.
stepper :: (Integral a) => a -> a -> [a]
stepper from to | from == to = [from]
                | from < to  = from : stepper (from+1) to
                | otherwise  = from : stepper (from-1) to

-- |Returns list of ordered floating-point random numbers. Returns n
-- numbers in range of lower <= xs < upper. I proved this algorithm
-- while I was freshman and I lost the paper somewhere. Do not try
-- flying to the Moon with these numbers. :-)
orderedRandom lower upper n gen = fix lower upper $ randomPositions n gen

fix _ _ [] = []
fix lower upper (x:xs) = new : fix new upper xs
    where new = (upper-lower)*x + lower

-- |This is not a mathematically proven algorithm for getting ordered
-- integers. It suffers from rounding problems when integer range is
-- small and many integers are being randomized. But it gives better
-- results than hand-picking the integers and requires no temporary
-- storage.
orderedIntegers :: (Integral a) => a -> a -> a -> StdGen -> [a]
orderedIntegers lower upper n gen = fixInt lower (upper-n+2) $ randomPositions n gen

--fixInt :: (Integral a, Random b, Floating b) => a -> a -> b -> [a]
fixInt :: (Integral a) => a -> a -> [Double] -> [a]
fixInt _ _ [] = []
fixInt lower upper (x:xs) = new : fixInt (new+1) (upper+1) xs
    where new = floor ((fromIntegral (upper-lower)) * x) + lower

-- |Returns bit map with length of n with 'trues' number of true
-- |values.
randomBitmap :: Int -> Int -> StdGen -> [Bool]
randomBitmap n trues gen | trues > n = replicate n True
                         | otherwise = match [1..n] ints
  where ints = orderedIntegers 1 n trues gen

match :: (Eq t) => [t] -> [t] -> [Bool]
match [] _  = []
match is [] = map (const False) is
match (i:is) (r:rs) | i==r      = True:match is rs
                    | otherwise = False:match is (r:rs)
                                  
-- |Pick given number of elements randomly from the list. The list
-- must be finite.
pick :: Int -> [a] -> StdGen -> [a]
pick n xs gen = pickTrues (randomBitmap (length xs) n gen) xs
    where pickTrues [] _ = []
          pickTrues (True:bs) (x:xs) = x:pickTrues bs xs
          pickTrues (False:bs) (_:xs) = pickTrues bs xs

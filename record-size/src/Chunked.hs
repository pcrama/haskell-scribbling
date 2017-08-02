module Chunked (
  mapFoldr1Chunked
)

where

import Data.Array ((!), Array, bounds, elems, listArray)

-- Prevent overflow by returning one less than the array size
numElements_1 :: Array Int e -> Int
numElements_1 a = let (min, max) = bounds a
                  in max - min

{- test with
((   Just $ listArray (0, 2) [9, 12, 15])
  == (mapFoldr1Chunked 3 (+) $ listArray (0, 8) [ 0, 1, 2
                                                , 3, 4, 5
                                                , 6, 7, 8]))
-}
-- Cut up input array into at least 2 chunks of at least 2 elements
-- each (the last incomplete chunk is ignored).  Imagine the chunks as
-- horizontal lines (i.e. rows of a 2D matrix), stacked below each
-- other.  Now foldr (foldr1 because there is always at least one
-- input since there are at least 2 chunks) over the sequence of
-- elements by looking at a column).  Repeat the same foldr over all
-- columns and collect the results in a row (i.e. map).
mapFoldr1Chunked :: Int -> (e -> e -> e) -> Array Int e -> Maybe (Array Int e)
mapFoldr1Chunked n f a | n < 2 || n > ((numElements_1 a - 1) `div` 2) + 1
                       -- There should at least be 2 complete chunks in the input
                       -- Note that this check also guards against overflows in
                       -- the code below
                       --
                       -- The check is written specially to avoid overflow (i.e.
                       -- divide RHS instead of multiplying LHS):
                       -- n = 5, bounds = (0, 9) [10 elements]
                       -- n > ((9 - 1) `div` 2) + 1 === n > 5 is False
                       -- n = 5, bounds = (0, 10) [11 elements]
                       -- n > ((10 - 1) `div` 2) + 1 === n > 5 is False, too
                       = Nothing
                       | otherwise = Just $ go n f a
  where go n f a = let chunkCount = ((numElements_1 a - (n - 1)) `div` n) + 1
                   in listArray (0, n - 1)
                              $ map (doFold1 n f a (chunkCount - 2) 0)
                                    [0..n-1]
        doFold1 n f a max_mul mul offs = let rhs = if mul >= max_mul
                                                   -- prior check of n vs. numElements
                                                   -- guards against overflow here:
                                                   then a ! (n * (mul + 1) + offs)
                                                   else doFold1 n f a max_mul (mul + 1) offs
                                       in f (a ! (n * mul + offs)) rhs

module Terrain (
  R1st2D,
  r12d,
  rowCount,
  colCount,
  rowFirst2DArray,
)
where

import Data.Array.Unboxed (UArray, array, bounds, (!))


data R1st2D = R1st2D Int (UArray Int Int)


rowFirst2DArray :: Int -- ^ row count
                -> Int -- ^ column count
                -> [(Int, Int, Int)] -- ^ list of (row, column, value) associations
                -> R1st2D
rowFirst2DArray 0 _ _ = error "Number of rows must be higher than 0"
rowFirst2DArray _ 0 _ = error "Number of columns must be higher than 0"
rowFirst2DArray rows cols assocs =
  R1st2D cols $ array (0, rows * cols - 1) [(r * cols + c, e) | (r, c, e) <- assocs]


rowCount :: R1st2D -- ^ 2-D array
         -> Int -- ^ row count
rowCount (R1st2D cols a) = (top - bot + 1) `div` cols
  where (bot, top) = bounds a


colCount :: R1st2D -- ^ 2-D array
         -> Int -- ^ column count
colCount (R1st2D cols _) = cols


r12d :: R1st2D -> Int -> Int -> Int
r12d (R1st2D cols a) r c = a ! (r * cols + c)

module Main where

import Control.Monad (guard)
import Data.Array (elems)

import Categories
import Chunked
import Utils

-- plausibleRecordSizes :: (CodePoint c, Foldable f) => Int -> f c -> [Interpretation]
plausibleRecordSizes maxSize s = do
  let cats = toNarrowCategoriesArray s
  -- heuristic: first try record sizes that are multiples of 4
  recordSize <- [4, 8..maxSize] ++ [x | x <- [2..maxSize], x `mod` 4 /= 0]
  let narrowInfo = mapFoldr1Chunked recordSize widen cats
  -- backtrack if narrowInfo is Nothing:
  guard $ maybe False (const True) narrowInfo
  let rle = maybe []
                  (rleCompress . elems)
                  narrowInfo
  -- Break if there are less than two possible categories or if there
  -- isn't both at least one possible number field and one possible
  -- text field:
  guard $ case rle of
            (_:_:_) -> -- at least two elements, now check numbers & text:
              any (canBeNumeric . fst) rle && any (canBeTextual . fst) rle
            otherwise -> False
  return rle

main :: IO ()
main = mapM_ (putStrLn . showRLE)
     $ removeMultiples
     $ plausibleRecordSizes 25
                            "hello, world123\
                            \world, hello456\
                            \five5_+sizty789\
                            \HELL WOOR,LD123\
                            \WORLD, HELLO456\
                            \FIVE5_+SIXTY789\
                            \tuvwxyz/{|}=131"
  where showRLE x = show (sum $ map snd x) ++ ": " ++ show x
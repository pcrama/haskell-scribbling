module Main where

import Categories
import Chunked

main :: IO ()
main = do
  let s = toNarrowCategoriesArray "hello123world456five5789"
  flip mapM_ [2..10] $ \recordSize ->
    putStrLn . show $ mapFoldr1Chunked recordSize widen s

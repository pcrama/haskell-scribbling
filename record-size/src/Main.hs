module Main where

import Control.Monad (guard)
import Data.Array (elems)
import Data.Monoid (First(..), getFirst, mconcat)
import System.Directory (getDirectoryContents) -- (listDirectory) -- GHC 8.0.1 only?

import Categories
import Chunked
import Utils

type Interpretation = [(NarrowestCategory, Int)]

plausibleRecordSize cats recordSize = do
  narrowInfo <- mapFoldr1Chunked recordSize widen cats
  let rle = rleCompress $ elems narrowInfo
  -- Break if there are less than two possible categories or if there
  -- isn't both at least one possible number field and one possible
  -- text field:
  guard $ case rle of
            (_:_:_) -> -- at least two elements, now check numbers & text:
              any (canBeNumeric . fst) rle && any (canBeTextual . fst) rle
            otherwise -> False
  return rle

guessRecordSize maxSize cats = getFirst
                             $ mconcat
                             $ map (First . plausibleRecordSize cats) [2..maxSize]

workWithFile :: FilePath -> IO Interpretation
workWithFile f = do
  content <- loadFile f
  return $ maybe [] id $ guessRecordSize 256 content

showInterpretation :: Interpretation -> String
showInterpretation x = (show $ sum $ map snd x) ++ ": " ++ show x

outputForFile :: FilePath -> Interpretation -> IO ()
outputForFile f i = do
  putStr f
  putStr ": "
  putStrLn $ case i of
               [] -> "No solution found.  Increase max size?"
               otherwise -> showInterpretation i

listDirectory :: FilePath -> IO [FilePath]
listDirectory f = do
    contents <- getDirectoryContents f
    return [x | x <- contents, not $ dotOrDotDot x]
  where dotOrDotDot "." = True
        dotOrDotDot ".." = True
        dotOrDotDot _ = False

main :: IO ()
main = do
  files <- listDirectory "./"
  mapM_ (\f -> workWithFile f >>= outputForFile f) files

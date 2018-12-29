module Main where

import Data.Function (on)
import Data.List (minimumBy)

import ParseRipSpec
import Shell

main :: IO ()
main = do
    inputString <- getContents
    putStrLn $ case readP_to_S parseSpec inputString of
      [(x, "")] -> unlines $ shellCommands $ translateSpec x
      [(x, y)] -> "Parse error in " ++ show y ++ " after\n" ++ formatSpec x
      lst@(_:_) -> "Parse error followed by " ++ (
        show $ minimumBy (compare `on` length) lst)
      _ -> "Unknown parse error"

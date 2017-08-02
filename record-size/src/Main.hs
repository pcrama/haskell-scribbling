module Main where

import Categories

main :: IO ()
main = do
  putStrLn $ "hello world" ++ (show $ narrowestCategory ' ')

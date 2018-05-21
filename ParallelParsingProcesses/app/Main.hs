module Main where

import PropLogic

testParseForm :: String -> IO ()
testParseForm s = do
  putStrLn s
  mapM_ (putStrLn . ("   "++) . show) $ parse form s

main :: IO ()
main = mapM_ testParseForm [
    "a&-b&-(-c&d)"
  , "a&b&c&d&e"
  , "-----a"
  , "--z--a" ]
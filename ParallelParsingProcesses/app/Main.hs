module Main where

import PropLogic
import P7ErrorReporting (parseLongest, parseWithPos)

testParseForm :: String -> IO ()
testParseForm s = do
  putStrLn s
  -- mapM_ (putStrLn . ("   "++) . show) $ parseLongest form s
  -- putStrLn . ("   "++) . show $ parseComplete form s
  putStrLn . ("   "++) . show $ parseLongest form s
  putStrLn . ("   "++) . show $ parseWithPos form (0 :: Int) (\p _ -> p + 1) s

main :: IO ()
main = do
  putStrLn $ "Using src/" ++ moduleName
  mapM_ testParseForm [
      "a&-b&-(-c&d)"
    , "a&b&c&d&e"
    , "-----a"
    , "-classic&(gun&gun)&(rose&rose"
    , "((gun&gun)&(rose&rose))&-classic"
    , "--z--a" ]

module Main where
import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V

import qualified Lib (someFunc, name, salary)

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
    Lib.someFunc
    csvData <- BL.readFile "salaries.csv"
    case decodeByName csvData of
        Left err -> putStrLn err
        Right (_, v) -> V.forM_ v $ \ p ->
            putStrLn $ Lib.name p ++ " earns " ++ show (Lib.salary p) ++ " dollars"

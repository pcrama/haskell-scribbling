module Lib (someFunc, Person(..)) where

import Data.Csv

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Person = Person
    { name   :: !String
    , salary :: !Int
    }

instance FromNamedRecord Person where
    parseNamedRecord r = Person <$> r .: "name" <*> r .: "salary"

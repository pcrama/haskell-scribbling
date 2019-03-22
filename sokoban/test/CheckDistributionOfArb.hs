module Main where

import Control.Monad (forM)
import Data.List (sort)
import Test.QuickCheck

import Game
import TestGame

putLabelled :: Show a => String -> a -> IO ()
putLabelled lbl x = putStrLn $ lbl ++ "=" ++ show x

trivialLevel :: Map -> [TestLevelCalls] -> Bool
trivialLevel initialMap logs =
     won initialMap
  -- playerMoves == 0 implies tileChanges == 0, so this check is not necessary:
  -- || 0 == playerMoves initialMap logs
  || 0 == tileChanges initialMap logs

data Stat a = Stat { p0 :: a, p25 :: a, p50 :: a, p75 :: a, p100 :: a }

instance Show a => Show (Stat a) where
  show (Stat { p0 = q0, p25 = q25, p50 = q50, p75 = q75, p100 = q100 }) =
    "Stat(" ++ show q0 ++ " " ++ show q25 ++ "/" ++ show q50 ++ "/" ++ show q75 ++ " " ++ show q100 ++ ")"

stats :: Ord a => [a] -> Maybe (Stat a)
stats as@(_:_:_:_:_) = Just $ go $ sort as
  where len = length as
        go xs = Stat { p0 = head xs
                     , p25 = xs !! (len `div` 4)
                     , p50 = xs !! (len `div` 2)
                     , p75 = xs !! ((3 * len) `div` 4)
                     , p100 = last xs }
stats _ = Nothing

count :: (a -> Bool) -> [a] -> Int
count f = foldr update (0 :: Int)
  where update x c | f x = c + 1
                   | otherwise = c

main :: IO ()
main = do
  putStrLn "(won?, remaining cmds, log length, scenario length, map size, trivial)"
  x <- forM [1..1000 :: Int] $ \i -> do
         ArbMap am <- generate arbitrary
         arbPcList <- generate arbitrary
         let (b, tls, logs) = runPlayLevelScenario am $ map Query $ extractPlayerCommands arbPcList
         let trivial = trivialLevel am logs
         let tiles = _rows am * _cols am
         putLabelled (show i) $ (b
                                , length tls
                                , length logs
                                , length arbPcList
                                , (_rows am, _cols am)
                                , trivial)
         return $ (length logs, b, trivial, tiles)
  putStrLn $ "Distribution of lengths of logs: " ++ (show $ stats $ map (\(a, _, _, _) -> a) x)
  putStrLn $ "Number of won levels: " ++ show (count id $ map (\(_, a, _, _) -> a) x)
  putStrLn $ "Number of won non trivial levels: " ++ show (count id $ map (\(_, w, t, _) -> w && not t) x)
  putStrLn $ "Number of trivial levels: " ++ show (count id $ map (\(_, _, a, _) -> a) x)
  putStrLn $ "Number of tiles: " ++ (show $ stats $ map (\(_, _, _, a) -> a) x)
  return ()

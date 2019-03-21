module Main where

import Control.Monad (forM)
import Test.QuickCheck

import Game
import TestGame

putLabelled :: Show a => String -> a -> IO ()
putLabelled lbl x = putStrLn $ lbl ++ "=" ++ show x

-- Prelude foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
playerMoves :: Map -> [TestLevelCalls] -> Int
playerMoves initialMap = snd . foldr update (initPos, 0)
  where initPos = _player initialMap
        update (Query _) x = x
        update (TestLevelFailure _) x = x
        update (Draw _ newPos) x@(prevPos, count)
          | newPos == prevPos = x
          | otherwise         = (newPos, count + 1)

crateMoves :: Map -> [TestLevelCalls] -> Int
crateMoves initialMap = snd . foldr update (initTiles, 0)
  where initTiles = [[_moveMap initialMap $ Pos { _x = c, _y = r }
                     | c <- [0.._cols initialMap - 1]]
                    | r <- [0.._rows initialMap - 1]]
        update (Query _) x = x
        update (TestLevelFailure _) x = x
        update (Draw newTiles _) x@(prevTiles, count)
          | newTiles == prevTiles = x
          | otherwise             = (newTiles, count + 1)

trivialLevel :: Map -> [TestLevelCalls] -> Bool
trivialLevel initialMap logs = 
     won initialMap
  -- playerMoves == 0 implies tileChanges == 0, so this check is not necessary:
  -- || 0 == playerMoves initialMap logs
  || 0 == tileChanges initialMap logs

main :: IO ()
main = do
  putStrLn "(won?, remaining cmds, log length, scenario length, map size, trivial)"
  _ <- forM [1..1000 :: Int] $ \i -> do
         ArbMap am <- generate arbitrary
         arbPcList <- generate arbitrary
         let (b, tls, logs) = runPlayLevelScenario am $ map Query $ extractPlayerCommands arbPcList
         putLabelled (show i) $ (b
                                , length tls
                                , length logs
                                , length arbPcList
                                , (_rows am, _cols am)
                                , trivialLevel am logs)
  return ()

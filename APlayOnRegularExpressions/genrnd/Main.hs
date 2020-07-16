module Main
where

import System.Environment (getArgs)
import System.Random      (getStdRandom, randomR)
import Text.Read          (readMaybe)

usage :: IO ()
usage = putStrLn "\
  \genrnd distance multiplier\n\
  \\n\
  \Generate random string of a/b, (distance + 1) * (multiplier + 1)\n\
  \long, such that there is never a pair of 'a' with distance \n\
  \characters in between.\n\
  \E.g. genrnd 0 5 could generate abbbab.\n\
  \\n\
  \genrnd [-a|-aa] n\n\
  \\n\
  \Print n * 'a' or n * 'aa'"

unfoldrM :: Monad m => (s -> m (Maybe (a, s))) -> s -> m [a]
unfoldrM g s0 = do
  x <- g s0
  case x of
    Just (a, s1) -> fmap (a:) $ unfoldrM g s1
    Nothing -> return []

data GenRndState = GenRndState {
  stillToDo :: Int
  , queue :: [Char]
  , stack :: [Char]
  }

step :: Monad m => m Char -> GenRndState -> m (Maybe (Char, GenRndState))
step g s@(GenRndState { stillToDo=todo, queue=alreadyGenerated, stack=detareneGydaerla })
  | todo <= 0 = return Nothing
  | otherwise = case alreadyGenerated of
      [] -> step g $ s { queue=reverse detareneGydaerla, stack=[] }
      (q:qs) -> do
        c <- if q == 'a' then return 'b' else g
        return $ Just (c, s { stillToDo=todo - 1
                            , queue=qs
                            , stack=c:detareneGydaerla })

genrnd :: Monad m => Int -> Int -> m Char -> m [Char]
genrnd distance multiplicator gen =
  unfoldrM (step gen)
         $ GenRndState { stillToDo=(distance + 1) * (multiplicator + 1)
                       , queue=replicate (distance + 1) 'b'
                       , stack=[] }

genC :: IO Char
genC = do
  x <- getStdRandom $ randomR (0, 1)
  return $ if x == (0 :: Int) then 'a' else 'b'

genAs :: Int -> IO ()
genAs n = putStrLn $ replicate n 'a'

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-a", howManyAs] -> case readMaybe howManyAs of
        Just count
          | count > 0 -> genAs count
          | otherwise -> usage
        Nothing -> usage
    ["-aa", howManyDoubleAs] -> case readMaybe howManyDoubleAs of
        Just count
          | count > 0 -> genAs $ 2 * count
          | otherwise -> usage
        Nothing -> usage
    [distS, multS] -> case (readMaybe distS, readMaybe multS) of
      (Just distance, Just multiplier) ->
        genrnd distance multiplier genC >>= mapM_ putChar
      _ -> usage
    _ -> usage
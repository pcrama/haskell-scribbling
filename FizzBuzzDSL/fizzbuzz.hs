-- Maciej Piróg: FizzBuzz in Haskell by Embedding a Domain-Specific Language
--
-- Monad.Reader Issue 23 (April 23, 2014)

module Main where

testFizzBuzz :: (Int -> String) -> Bool
testFizzBuzz f = and $ map test [
  (1, "1"), (2, "2"), (3, "Fizz"), (4, "4"), (5, "Buzz"),
  (6, "Fizz"), (7, "7"), (8, "8"), (9, "Fizz"), (10, "Buzz"),
  (11, "11"), (12, "Fizz"), (13, "13"), (14, "14"), (15, "FizzBuzz"),
  (16, "16"), (17, "17"), (18, "Fizz"), (19, "19"), (20, "Buzz"),
  (21, "Fizz"), (22, "22"), (23, "23"), (24, "Fizz"), (25, "Buzz"),
  (26, "26"), (27, "Fizz"), (28, "28"), (29, "29"), (30, "FizzBuzz"),
  (31, "31"), (32, "32")]
  where test (n, s) = f n == s

doTest :: String -> (Int -> String) -> IO ()
doTest name f = do
  putStr name
  putStr ": "
  putStrLn $ if testFizzBuzz f then "ok" else "!KO!"

fizzbuzz1 :: Int -> String
fizzbuzz1 n
  | (n `mod` 5 == 0) && (n `mod` 3 == 0) = "FizzBuzz"
  | n `mod` 3 == 0 = "Fizz"
  | n `mod` 5 == 0 = "Buzz"
  | otherwise = show n

fizzbuzz2 :: Int -> String
fizzbuzz2 n
  | n `mod` 3 == 0 = "Fizz" ++ (if n `mod` 5 == 0 then "Buzz" else "")
  | n `mod` 5 == 0 = "Buzz"
  | otherwise = show n

fizzbuzz3 :: Int -> String
fizzbuzz3 n = ((if n `mod` 3 == 0 then "Fizz" else "")
               ++ (if n `mod` 5 == 0 then "Buzz" else "")
              ) <| show n
  where "" <| x = x
        a <| _ = a

data Command = Skip | Print String | Halt

printInt :: Int -> Command
printInt = Print . show

type Program = [Command]

type Context = Program -> Program

base :: Int -> Context
base n = (++ [printInt n])

fizz :: Int -> Context
fizz n
  | n `mod` 3 == 0 = \p -> [Print "Fizz"] ++ p ++ [Halt]
  | otherwise = id

buzz :: Int -> Context
buzz n
  | n `mod` 5 == 0 = \p -> [Print "Buzz"] ++ p ++ [Halt]
  | otherwise = id

step :: Command -> String -> String
step Halt _ = ""
step Skip x = x
step (Print s) x = s ++ x

-- Direct definition (as a fold)
fizzbuzz4 :: Int -> String
fizzbuzz4 n = eval $ (base n . fizz n . buzz n) [Skip]
  where eval = foldr step ""

fizzbuzzhisshowl :: Int -> Context
fizzbuzzhisshowl n =
    base n . mkCtx 3 "Fizz" . mkCtx 5 "Buzz" . mkCtx 7 "Hiss" . mkCtx 11 "Howl"
  where mkCtx :: Int -> String -> Context
        mkCtx m s p
          | n `mod` m == 0 = [Print s] ++ p ++ [Halt]
          | otherwise = p

-- exercise 3
-- foldr step "" p = foldr (.) id (fmap step p) ""
--
-- Base case:
--   foldr step "" [] = ""
--   foldr (.) id (fmap step []) "" = foldr (.) id [] "" = id "" = ""
--   
-- One step (for lists, but Foldable can be made into a list anyway):
--   foldr step "" (p:ps) = step p $ foldr step "" ps
--   foldr (.) id (fmap step $ p:ps) ""
--     = foldr (.) id (step p:(fmap step:ps)) ""
--     = (step p . (foldr (.) id (fmap step ps))) ""
--     = step p $ foldr (.) id (fmap step ps) ""
--   = step p $ foldr step "" ps

fizzbuzz5 :: Int -> String
fizzbuzz5 n = eval $ (base' . fizz' . buzz') [step Skip]
  where eval :: [String -> String] -> String
        eval p = foldr (.) id p ""
        base' p = p ++ [step $ printInt n]
        fizz' = mkCtx 3 "Fizz"
        buzz' = mkCtx 5 "Buzz"
        mkCtx q s p
          | n `mod` q == 0 = [step $ Print s] ++ p ++ [step Halt]
          | otherwise = p

-- step (Print s) == (s ++)
-- step Skip = id
-- step Halt = const ""
fizzbuzz6 :: Int -> String
fizzbuzz6 n = (base' . fizz' . buzz') stepSkip ""
  where base' p = p . stepPrintInt n
        fizz' = mkCtx 3 "Fizz"
        buzz' = mkCtx 5 "Buzz"
        mkCtx q s p
          | n `mod` q == 0 = stepPrint s . p . stepHalt
          | otherwise = p
        stepPrintInt = stepPrint . show
        stepPrint s t = s ++ t
        stepSkip = id
        stepHalt = const ""

-- Inlining, final polishing
fizzbuzz7 :: Int -> String
fizzbuzz7 n = (test 3 "Fizz" . test 5 "Buzz") id $ show n
  where test :: Int -> String -> (String -> String) -> String -> String
        test q s p t
          | n `mod` q == 0 = s ++ p ""
          | otherwise = p t

mkFizzBuzzEtc :: [(Int, String)] -> Int -> String
mkFizzBuzzEtc spec n = foldr (.) id specList id $ show n
  where specList :: [(String -> String) -> (String -> String)]
        specList = map (uncurry test) spec
        test :: Int -> String -> (String -> String) -> String -> String
        test q s p t
          | n `mod` q == 0 = s ++ p ""
          | otherwise = p t

main :: IO ()
main = do
  doTest "fizzbuzz1" fizzbuzz1
  doTest "fizzbuzz2" fizzbuzz2
  doTest "fizzbuzz3" fizzbuzz3
  doTest "fizzbuzz4" fizzbuzz4
  doTest "fizzbuzz5" fizzbuzz5
  doTest "fizzbuzz6" fizzbuzz6
  doTest "fizzbuzz7" fizzbuzz7
  putStrLn $ mkFizzBuzzEtc [(3, "Fizz"), (5, "Buzz"), (7, "Hiss"), (11, "Howl")] 55

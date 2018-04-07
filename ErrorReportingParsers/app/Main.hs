-- Based on: Error Reporting Parsers: a Monad Transformer Approach
-- by Matt Fenwick & Jay Vyas following along
-- https://themonadreader.files.wordpress.com/2013/08/issue221.pdf
module Main where

import Control.Applicative (Applicative(..), Alternative(..))
-- import SimpleParser
import MTParser1

data Nesting = One Char | Many [Nesting] deriving (Show, Eq)

char :: Parser Char Nesting
char = fmap One $ check (not . (`elem` "()")) item

level :: Parser Char Nesting
level = literal '(' *> fmap Many (many element) <* literal ')'

element :: Parser Char Nesting
element = char <|> level

parseNest = many element

p :: Parser a (a, a)
p = (,) <$> item <*> item

q :: Eq a => a -> Parser a (a, a)
q x = item >>= (\y -> if x == y then return (x, x) else fmap ((,) y) item)

testParser :: (Show t, Show a) => String -> Parser t a -> [t] -> IO ()
testParser s p ts =
  putStrLn $ s ++ " " ++ show ts ++ " = " ++ (show $ getParser p ts)

main = do
  testParser "p" p [1]
  testParser "q 1" (q 1) [1]
  flip mapM_ [[1], [2], [3, 4], [5]] $ testParser "q 1 <|> q 2" (q 1 <|> q 2)
  flip mapM_ [[1, 2], [1, 3, 5], [1], [2], [3], [], [4, 5, 6]] $ testParser "fmap (+) (check (==1)) <*> check (==3)" ((+) <$> check (==1) item <*> check (==3) item)
  flip mapM_ ["abc", "(ab)", "((((a)b))", "((a))b"] $ testParser "parseNest" parseNest

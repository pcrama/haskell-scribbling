module SimpleParser (
  Parser(..)
  , item
  , check
  , literal
  ) where

-- Based on: Error Reporting Parsers: a Monad Transformer Approach
-- by Matt Fenwick & Jay Vyas following along (pp 41-46)
-- https://themonadreader.files.wordpress.com/2013/08/issue221.pdf
import Control.Applicative (Applicative(..), Alternative(..))

newtype Parser t a = Parser { getParser :: [t] -> Maybe ([t], a) }

item = Parser f
  where f [] = Nothing
        f (x:xs) = Just (xs, x)

instance Functor (Parser t) where
  fmap f (Parser p) = Parser $ fmap (fmap f) . p

instance Applicative (Parser t) where
  pure r = Parser $ \xs -> Just (xs, r)
  (Parser f) <*> (Parser x) = Parser $ go f x
    where go :: ([t] -> Maybe ([t], a -> b)) -> ([t] -> Maybe ([t], a)) -> [t] -> Maybe ([t], b)
          go f x xs =
            case f xs of
              Nothing -> Nothing
              Just (ys, f') -> fmap (fmap f') $ x ys

instance Monad (Parser t) where
  return = pure
  (Parser a) >>= f =
    Parser $ \ts -> a ts >>= \(ts', x) -> getParser (f x) ts'
-- Written `explicitly'
--  (Parser a) >>= f = Parser $ go a f
--    where go :: ([t] -> Maybe ([t], a)) -> (a -> Parser t b)
--             -> ([t] -> Maybe ([t], b))
--          go a f ts = case a ts of
--                        Nothing -> Nothing
--                        Just (ts', a') -> getParser (f a') ts'

instance Alternative (Parser t) where
  empty = Parser $ const empty
  (Parser a) <|> (Parser b) = Parser $ \ts -> a ts <|> b ts

check :: (Monad f, Alternative f) => (a -> Bool) -> f a -> f a
check p fa = fa >>= (\a -> if p a then return a else empty)

literal :: Eq a => a -> Parser a a
literal a = check (==a) item

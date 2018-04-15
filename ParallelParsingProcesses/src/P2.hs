{-# LANGUAGE ExistentialQuantification #-}
-- Implements naive term representation of
-- See http://www.cse.chalmers.se/edu/course/course/afp/Papers/parser-claessen.pdf
module P2
    ( symbol
    , P
    , parse
    , (+++)
    ) where

import Control.Applicative (Alternative(..))

data P i o = Symbol (i -> o)
           | Fail
           | P i o :+++ P i o
           | forall b . P i b :>>= (b -> P i o)
           | Return o

parse :: P i o -> [i] -> [([i], o)]
parse (Symbol _) [] = []
parse (Symbol f) (x:xs) = [(xs, f x)]
parse Fail _ = []
parse (p :+++ q) s = parse p s ++ parse q s
parse (p :>>= f) s = concatMap go $ parse p s
  where go (s, a) = parse (f a) s
parse (Return x) s = [(s, x)]

symbol :: P i i
symbol = Symbol id

instance Functor (P i) where
  fmap f (Symbol g) = Symbol $ f . g
  fmap _ Fail = Fail
  fmap f (p :+++ q) = (fmap f p) :+++ (fmap f q)
  fmap f (p :>>= k) = p :>>= ((fmap f) . k)
  fmap f (Return x) = Return $ f x

instance Applicative (P i) where
  pure = Return
  -- P i (a -> b) <*> P i a = P i b
  p <*> q = p :>>= \f -> fmap f q

instance Monad (P i) where
  fail _ = Fail
  return = Return
  (>>=) = (:>>=)

(+++) :: P i o -> P i o -> P i o
(+++) = (:+++)

instance Alternative (P i) where
  empty = Fail
  (<|>) = (+++)

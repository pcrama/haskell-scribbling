-- Section 6 of
-- http://www.cse.chalmers.se/edu/course/course/afp/Papers/parser-claessen.pdf
-- Implementation B: Removing Bind
module P3RemovingBind
    ( symbol
    , P
    , parse
    , (+++)
    , moduleName
    ) where

import Control.Applicative (Alternative(..))

moduleName :: String
moduleName = "P3RemovingBind"

data P i o = Fail
           | SymbolBind (i -> P i o)
           | P i o :+++ P i o
           | Return o

parse :: P i o -> [i] -> [([i], o)]
parse (SymbolBind _) [] = []
parse (SymbolBind k) (x:xs) = parse (k x) xs
parse Fail _ = []
parse (p :+++ q) s = parse p s ++ parse q s
parse (Return x) s = [(s, x)]

symbol :: P i i
symbol = SymbolBind pure

instance Functor (P i) where
  fmap f (SymbolBind k) = SymbolBind $ (fmap f) . k
  fmap _ Fail = Fail
  fmap f (p :+++ q) = (fmap f p) :+++ (fmap f q)
  fmap f (Return x) = Return $ f x

instance Applicative (P i) where
  pure = Return
  -- P i (a -> b) <*> P i a = P i b
  p <*> q = p >>= \f -> fmap f q

instance Monad (P i) where
  fail _ = Fail
  return = pure
  (SymbolBind f) >>= k = SymbolBind $ \x -> f x >>= k
  Fail >>= _ = Fail
  (p :+++ q) >>= f = (p >>= f) :+++ (q >>= f)
  (Return o) >>= f = f o

(+++) :: P i o -> P i o -> P i o
(+++) = (:+++)

instance Alternative (P i) where
  empty = Fail
  (<|>) = (+++)

-- Section 7 of
-- http://www.cse.chalmers.se/edu/course/course/afp/Papers/parser-claessen.pdf
-- Implementation C: Removing Plus
module P4RemovingPlus
    ( symbol
    , P(..) -- exporting constructors only for P5AssociativityOfBind
    , parse
    , (+++)
    , moduleName
    ) where

import Control.Applicative (Alternative(..))

moduleName :: String
moduleName = "P4RemovingPlus"

data P i o = Fail
           | SymbolBind (i -> P i o)
           | ReturnPlus o (P i o) -- ReturnPlus o p === return o +++ p

parse :: P i o -> [i] -> [([i], o)]
parse (SymbolBind _) [] = []
parse (SymbolBind k) (x:xs) = parse (k x) xs
parse Fail _ = []
parse (ReturnPlus o p) xs = (xs, o):parse p xs

symbol :: P i i
symbol = SymbolBind pure

instance Functor (P i) where
  fmap f (SymbolBind k) = SymbolBind $ (fmap f) . k
  fmap _ Fail = Fail
  fmap f (ReturnPlus o p) = ReturnPlus (f o) (fmap f p)

instance Applicative (P i) where
  pure = flip ReturnPlus Fail
  -- P i (a -> b) <*> P i a = P i b
  p <*> q = p >>= \f -> fmap f q

instance Monad (P i) where
  fail _ = Fail
  return = pure
  (SymbolBind f) >>= k = SymbolBind $ \x -> f x >>= k
  Fail >>= _ = Fail
  (ReturnPlus o p) >>= f = f o +++ (p >>= f)

(+++) :: P i o -> P i o -> P i o
Fail +++ p = p
p +++ Fail = p
(SymbolBind k) +++ (SymbolBind k') = SymbolBind $ \c -> k c +++ k' c
(ReturnPlus a p) +++ (ReturnPlus b q) = ReturnPlus a $ ReturnPlus b $ p +++ q
(ReturnPlus o p) +++ sb@(SymbolBind _) = ReturnPlus o $ p +++ sb
sb@(SymbolBind _) +++ (ReturnPlus o p) = ReturnPlus o $ p +++ sb

instance Alternative (P i) where
  empty = Fail
  (<|>) = (+++)

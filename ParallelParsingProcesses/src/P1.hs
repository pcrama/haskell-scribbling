{-# LANGUAGE DeriveFunctor #-}

-- See http://www.cse.chalmers.se/edu/course/course/afp/Papers/parser-claessen.pdf
module P1
    ( symbol
    , P(..)
    , (+++)
    , moduleName
    ) where

import Control.Applicative (Alternative(..))

moduleName :: String
moduleName = "P1"

newtype P i o = P { parse :: [i] -> [([i], o)] }
  deriving Functor

symbol :: P i i
symbol = P go
  where go [] = []
        go (x:xs) = [(xs, x)]

instance Applicative (P i) where
  pure x = P go
    where go s = [(s, x)]
  (P pf) <*> (P pa) = P go
    where go s = [ (s'', f' a'')
                 | (s', f') <- pf s
                 , (s'', a'') <- pa s' ]

instance Monad (P i) where
  fail _ = P $ const []
  return = pure
  (P pa) >>= k = P go
    where go s = [ (s'', x'')
                 | (s', a') <- pa s
                 , (s'', x'') <- parse (k a') s' ]

(+++) :: P i o -> P i o -> P i o
(P a) +++ (P b) = P go
  where go s = merge (a s) (b s)
        merge xs [] = xs
        merge [] ys = ys
        merge (x:xs) (y:ys) = x:y:merge xs ys

instance Alternative (P i) where
  empty = P $ const []
  (<|>) = (+++)

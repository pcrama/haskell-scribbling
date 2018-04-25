{-# LANGUAGE RankNTypes #-}
-- Section 9 of
-- http://www.cse.chalmers.se/edu/course/course/afp/Papers/parser-claessen.pdf
-- Implementation D: Associativity of Bind
module P5AssociativityOfBind
    ( symbol
    , P
    , parse
    , (+++)
    ) where

import Control.Applicative (Alternative(..))
import qualified P4RemovingPlus as P

type Context s a b = a -> P.P s b

type P s a = forall b . Context s a b -> P.P s b

data PP i o
  = SymbolBind (i -> PP i o)

-- D4. p === \k -> P.p (P.>>=) k
-- D5. parse p === P.parse P.p

-- symbol = \k -> P.symbol P.>>= k
--        = \k -> P.SymbolBind return P.>>= k
--        = \k -> 
symbol = SymbolBind
fail = 

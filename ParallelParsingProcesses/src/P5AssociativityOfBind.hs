{-# LANGUAGE RankNTypes #-}
-- Section 9 of
-- http://www.cse.chalmers.se/edu/course/course/afp/Papers/parser-claessen.pdf
-- Implementation D: Associativity of Bind
module P5AssociativityOfBind
    ( symbol
    , failp -- `fail' is already defined in Prelude -> renamed
    , returnplus
    , P
--    , parse
    , (+++)
    , moduleName
    ) where

-- import Control.Applicative (Alternative(..))
import qualified P4RemovingPlus as P

type Context s a b = a -> P.P s b

moduleName :: String
moduleName = "P5AssociativityOfBind"

type P s a = forall b . Context s a b -> P.P s b

-- D4. p === \k -> P.p (P.>>=) k
-- D5. parse p === P.parse P.p

-- symbol = \k -> P.symbol P.>>= k
--        = \k -> P.SymbolBind return P.>>= k
--        = \k -> P.SymbolBind k
symbol :: P i i
symbol = P.SymbolBind

-- failp = \k -> P.fail P.>>= k
--       = \k -> P.Fail >>= k
--       = const P.Fail
failp :: P i o
failp = const P.Fail

-- returnplus o p = \k -> (P.returnPlus o p) P.>>= k
--                = \k -> (return o P.+++ p) P.>>= k
--                = \k -> k o P.+++ p P.>>= k
-- returnplus :: o -> P i o -> P i o
returnplus :: o -> (forall b. Context i o b -> P.P i b) -> (forall c. Context i o c -> P.P i c)
returnplus o p k = k o P.+++ p k

(+++) :: P i o -> P i o -> P i o
a +++ b = \k -> a k P.+++ b k

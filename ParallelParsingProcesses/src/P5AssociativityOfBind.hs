{-# LANGUAGE RankNTypes #-}
-- Section 9 of
-- http://www.cse.chalmers.se/edu/course/course/afp/Papers/parser-claessen.pdf
-- Implementation D: Associativity of Bind
module P5AssociativityOfBind
    ( symbol
    , failp -- `fail' is already defined in Prelude -> renamed
    , returnplus
    , P
    , parse
    , (+++)
    , moduleName
    ) where

import Control.Applicative (Alternative(..))
import qualified P4RemovingPlus as P4

moduleName :: String
moduleName = "P5AssociativityOfBind"

type Context s a b = a -> P4.P s b

newtype P s a = P (forall b . Context s a b -> P4.P s b)

-- D4. p === \k -> P4.p (P4.>>=) k
-- D4  could also be written as p k === P4.p (P4.>>=) k
--
-- D5. parse p === P4.parse P4.p

-- symbol = \k -> P4.symbol P4.>>= k
--        = \k -> P4.SymbolBind return P4.>>= k
--        = \k -> P4.SymbolBind k
symbol :: P i i
symbol = P $ P4.SymbolBind

-- failp = \k -> P4.fail P4.>>= k
--       = \k -> P4.Fail >>= k
--       = const P4.Fail
failp :: P i o
failp = P $ const P4.Fail

-- returnplus o p = \k -> (P4.returnPlus o p) P4.>>= k
--                = \k -> (return o P4.+++ p) P4.>>= k
--                = \k -> k o P4.+++ p P4.>>= k
-- returnplus :: o -> P i o -> P i o
returnplus' :: o -> (forall b. Context i o b -> P4.P i b) -> (forall c. Context i o c -> P4.P i c)
returnplus' o p k = k o P4.+++ p k

returnplus :: o -> P i o -> P i o
returnplus o (P p) = P $ returnplus' o p

(+++) :: P i o -> P i o -> P i o
(P a) +++ (P b) = P $ \k -> a k P4.+++ b k

instance Functor (P s) where
  fmap f (P p) = P $ \k -> p (k . f)

-- The paper I use seems to have been written before Applicative was known,
-- so it is easier for me to write Applicative in terms of their Monad
-- instance.
instance Applicative (P s) where
  -- return x = \k -> (P4.return x) (P4.>>=) k
  --          = \k -> k x
  pure x = P $ \k -> k x
  -- P s (a -> b) <*> P s a -> P s b
  fab <*> p = do
                f <- fab
                a <- p
                return $ f a

instance Monad (P s) where
  -- return x = \k -> (P4.return x) (P4.>>=) k
  --          = \k -> k x
  return = pure
  -- p >>= f = \k -> (P4.p (P4.>>=) P4.f) (P4.>>=) k
  --         = \k -> P4.p (P4.>>=) (\x -> P4.f x (P4.>>=) k)
  --         = \k -> P4.p (P4.>>=) (P4.f x (P4.>>=) k)
  (P p) >>= f = P $ \k -> p (\x -> case f x of P p' -> p' k)
  fail = const failp

instance Alternative (P s) where
  empty = failp
  (<|>) = (+++)

parse (P p) = P4.parse $ p return

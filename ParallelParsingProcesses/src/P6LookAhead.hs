{-# LANGUAGE RankNTypes #-}
-- Look Ahead (Section 10 of Koen Claessen's Functional Pearl
-- Parallel Parsing Processes)
-- http://www.cse.chalmers.se/edu/course/course/afp/Papers/parser-claessen.pdf
module P6LookAhead
    ( symbol
    , failp -- `fail' is already defined in Prelude -> renamed
    , P
    , parse
    , (+++)
    , moduleName
    ) where

import Control.Applicative (Alternative(..))

moduleName :: String
moduleName = "P6LookAhead"

-- Copying implementation at the end of Section 9 and will modify it

data P' i o = Fail
            | SymbolBind (i -> P' i o)
            | ReturnPlus o (P' i o) -- ReturnPlus o p === return o ++++ p

instance Functor (P' i) where
  fmap f (SymbolBind sb) = SymbolBind $ fmap f . sb
  fmap _ Fail = Fail
  fmap f (ReturnPlus o pio) = ReturnPlus (f o) $ fmap f pio

instance Applicative (P' i) where
  pure = flip ReturnPlus Fail
  Fail <*> _ = Fail
  _ <*> Fail = Fail
  SymbolBind f <*> (ReturnPlus o pio) = (SymbolBind $ \i -> fmap ($ o) (f i))
                                   ++++ (SymbolBind f <*> pio)
  SymbolBind f <*> sb@(SymbolBind _) = SymbolBind $ \i -> f i <*> sb
  (ReturnPlus f pio) <*> pa@(ReturnPlus a pjp) = ReturnPlus (f a)
                                                          $ (fmap f pjp) ++++ (pio <*> pa)
  (ReturnPlus f pio) <*> pjp = fmap f pjp ++++ (pio <*> pjp)

instance Monad (P' i) where
  fail _ = Fail
  return = pure
  (SymbolBind f) >>= k = SymbolBind $ \x -> f x >>= k
  Fail >>= _ = Fail
  (ReturnPlus o p) >>= f = f o ++++ (p >>= f)

instance Alternative (P' i) where
  empty = Fail
  (<|>) = (++++)

newtype P i o = P (forall z . (o -> P' i z) -> P' i z)

instance Functor (P s) where
  -- fmap :: (o -> q)
  --      -> P (forall z . (o -> P' i z) -> P' i z)
  --      -> P (forall z . (q -> P' i z) -> P' i z)
  -- -- p :: (forall z . (o -> P' i z) -> P' i z)
  -- -- k :: q -> P' i z
  fmap f (P p) = P $ \k -> p (k . f)

instance Applicative (P s) where
  pure x = P $ \k -> k x
  p <*> q = p >>= \f -> fmap f q -- abusing Monad instance.  Paper must have been written before Applicative was a thing
  -- I tried these, but they turned into an endless loop:
  -- <*> :: P (forall z . ((a -> b) -> P' i z) -> P' i z)
  --     -> P (forall z . (a -> P' i z) -> P' i z)
  --     -> P (forall z . (b -> P' i z) -> P' i z)
  -- -- pf :: (forall z . ((a -> b) -> P' i z) -> P' i z)
  -- -- pa :: (forall z . (a -> P' i z) -> P' i z)
  -- -- k  :: b -> P' i z
  -- (P pf) <*> (P pa) = P $ \k -> (pf pure <*> pa pure) >>= k
  -- (P pf) <*> (P pa) = P $ \k -> go (pf pure) (pa pure) k
  --   where go Fail _ _ = Fail
  --         go _ Fail _ = Fail
  --         go (SymbolBind f) pa' k = SymbolBind $ \i -> go (f i) pa' k
  --         go (ReturnPlus fab pfab') pa' k =
  --           finish k (fmap fab pa') ++++ go pfab' pa' k
  --         finish _ Fail = Fail
  --         finish k (SymbolBind f) = SymbolBind $ \i -> finish k $ f i
  --         finish k (ReturnPlus b pb') = (k b) ++++ (pb' >>= k)
-- Applicative laws:
-- 1. pure f <*> pure x = pure $ f x
--
--    pure f <*> pure x
--  = (P $ \k1 -> k1 f) <*> (P $ \k2 -> k2 x) [[via definition of pure@P]]
--  = P $ \k3 -> go (pure f) (pure x) k3 [[RHS of @P<*>@P; filling in k1 & k2]]
--  = P $ \k3 -> go (ReturnPlus f Fail) (ReturnPlus x Fail) k3 [[definition of pure@P']]
--  = P $ \k3 -> finish (fmap f $ ReturnPlus x Fail) k3 ++++ go Fail (ReturnPlus x Fail) k3
--               [[definition of go]]
--  = P $ \k3 -> finish (fmap f $ ReturnPlus x Fail) k3 ++++ go Fail (ReturnPlus x Fail) k3
--               [[definition of fmap@P', twice]]            [[definition of go]]
--  = P $ \k3 -> finish (ReturnPlus (f x) Fail) k3 ++++ Fail
--  = P $ \k3 -> finish (ReturnPlus (f x) Fail) k3 [[definition of ++++]]
--  = P $ \k3 -> (k3 $ f x) ++++ finish Fail k3 [[definition of finish]]
--  = P $ \k3 -> (k3 $ f x) ++++ Fail [[definition of finish]]
--  = P $ \k3 -> k3 $ f x [[definition of ++++]]
--  = pure $ f x [[definition of pure@P]]

symbol :: P i i
symbol = P $ SymbolBind

failp :: P i o
failp = P $ const Fail

instance Monad (P i) where
  return = pure
  -- P i a >>= (a -> P i b) :: P i b
  -- p :: forall z . (a -> P' i z) -> P' i z
  -- f :: a -> P (forall z . (b -> P' i z) -> P' i z)
  -- k :: b -> P' i z
  -- x :: a
  (P p) >>= f = P $ \k -> p (\x -> case f x of P p' -> p' k)
  fail = const failp

instance Alternative (P i) where
  empty = failp
  (<|>) = (+++)

parse :: P i o -> [i] -> [([i], o)]
parse (P p) = parse' $ p pure

parse' :: P' i o -> [i] -> [([i], o)]
parse' (SymbolBind _) [] = []
parse' (SymbolBind k) (x:xs) = parse' (k x) xs
parse' Fail _ = []
parse' (ReturnPlus o p) xs = (xs, o):parse' p xs

(+++) :: P i o -> P i o -> P i o
(P a) +++ (P b) = P $ \k -> a k ++++ b k

(++++) :: P' i o -> P' i o -> P' i o
Fail ++++ p = p
p ++++ Fail = p
(SymbolBind k) ++++ (SymbolBind k') = SymbolBind $ \c -> k c ++++ k' c
(ReturnPlus a p) ++++ (ReturnPlus b q) = ReturnPlus a $ ReturnPlus b $ p ++++ q
(ReturnPlus o p) ++++ sb@(SymbolBind _) = ReturnPlus o $ p ++++ sb
sb@(SymbolBind _) ++++ (ReturnPlus o p) = ReturnPlus o $ sb ++++ p

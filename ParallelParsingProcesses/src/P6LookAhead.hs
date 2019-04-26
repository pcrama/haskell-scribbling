{-# LANGUAGE RankNTypes #-}
-- Look Ahead (Section 10 of Koen Claessen's Functional Pearl
-- Parallel Parsing Processes) and Other Parse Functions
-- (Section 11)
-- http://www.cse.chalmers.se/edu/course/course/afp/Papers/parser-claessen.pdf
module P6LookAhead
    ( symbol
    , failp -- `fail' is already defined in Prelude -> renamed
    , P
    , parse
    , parseComplete
    , parseLongest
    , parseWithPos
    , look
    , eof
    , try
    -- , longest -- I'm too ashamed of my implementation to export it
    , munch
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
            | LookBind ([i] -> P' i o)

instance Functor (P' i) where
  fmap f (SymbolBind sb) = SymbolBind $ fmap f . sb
  fmap f (LookBind lb) = LookBind $ fmap f . lb
  fmap _ Fail = Fail
  fmap f (ReturnPlus o pio) = ReturnPlus (f o) $ fmap f pio

instance Applicative (P' i) where
  pure = flip ReturnPlus Fail
  Fail <*> _ = Fail
  _ <*> Fail = Fail
  SymbolBind f <*> (ReturnPlus o pio) = (SymbolBind $ \i -> fmap ($ o) (f i))
                                   ++++ (SymbolBind f <*> pio)
  SymbolBind f <*> sb@(SymbolBind _) = SymbolBind $ \i -> f i <*> sb
  LookBind f <*> (ReturnPlus o pio) = (LookBind $ \i -> fmap ($ o) (f i))
                                 ++++ (LookBind f <*> pio)
  LookBind f <*> lb@(LookBind _) = LookBind $ \i -> f i <*> lb
  SymbolBind f <*> lb@(LookBind _) = SymbolBind $ \i -> f i <*> lb
  LookBind f <*> sb@(SymbolBind _) = LookBind $ \i -> f i <*> sb
  (ReturnPlus f pio) <*> pa@(ReturnPlus a pjp) = ReturnPlus (f a)
                                                          $ (fmap f pjp) ++++ (pio <*> pa)
  (ReturnPlus f pio) <*> pjp = fmap f pjp ++++ (pio <*> pjp)

instance Monad (P' i) where
  fail _ = Fail
  return = pure
  (SymbolBind f) >>= k = SymbolBind $ \x -> f x >>= k
  (LookBind f) >>= k = LookBind $ \x -> f x >>= k
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
  -- Not updated after adding LookBind
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
-- Applicative laws: (Not updated after adding LookBind)
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

look :: P s [s]
look = P $ LookBind

munch :: (s -> Bool) -> P s [s]
munch p = look >>= go
  where go (c:xs) | p c = do
                            _ <- symbol
                            fmap (c:) $ go xs
        go _ = return [] -- no input left or predicate not satisfied

eof :: P s ()
eof = do
        s <- look
        case s of
          [] -> return ()
          _ -> empty

-- I really don't see why `look' would be needed for this one, unless the
-- input should not be consumed.  The text is not clear enough on that for me.
try :: Alternative f => f a -> f (Maybe a)
try p = fmap Just p <|> pure Nothing

-- This is so ugly, I'm ashamed: it is SICP code (i.e. not idiomatic).
-- It even runs down the complete input to compute the length!
-- I commented it out after lightly testing it.
--
-- longest :: P i o -> P i o
-- longest p = do
--               s <- look
--               case parse p s of
--                 [] -> empty
--                 [(s', x)] -> finish s s' x
--                 manySolutions -> let (s', x) = pickShortestRemainder $ map (\(s', v) -> (s', (s', v)))
--                                                                            manySolutions
--                                  in finish s s' x
--   where -- longest match means that the remainder is the shortest
--         pickShortestRemainder sols = case findAssoc null sols of
--                                        Just (s', x) -> (s', x)
--                                        Nothing -> pickShortestRemainder $ trimSols sols
--         trimSols = map (\((_:xs), v) -> (xs, v))
--         findAssoc p = foldr (\(k, v) b -> if p k then Just v else b) Nothing
--         finish s s' v = finish' v $ length s - length s'
--         finish' v 0 = return v
--         finish' v n = symbol *> (finish' v $ n - 1)

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
parse' (LookBind k) s = parse' (k s) s
parse' Fail _ = []
parse' (ReturnPlus o p) xs = (xs, o):parse' p xs

-- Only returns first result for which the parse is complete (i.e. no input is left)
--
-- The paper user parseComplete :: P i o -> [i] -> [o] and returns all results
parseComplete :: P i o -> [i] -> Maybe o
parseComplete (P p) = go $ p pure
  where go (SymbolBind _) [] = Nothing
        go (SymbolBind k) (x:xs) = go (k x) xs
        go (LookBind k) s = go (k s) s
        go Fail _ = Nothing
        go (ReturnPlus o _) [] = Just o
        go (ReturnPlus _ p) xs = go p xs -- input not completely used up -> discard output

-- Only return (last) result using more of the input than all other results
--
-- The paper uses parseLongest :: P i o -> [i] -> Maybe (o, [i])
parseLongest :: P i o -> [i] -> ([i], Maybe o)
parseLongest (P p) xs = go (p pure) xs (xs, Nothing)
  where go (SymbolBind _) [] r = r
        go (SymbolBind k) (x:xs) r = go (k x) xs r
        go (LookBind k) s r = go (k s) s r
        go Fail xs r = r
        -- since we never go backwards in the input, any time we return
        -- a result, we must have used at least as much input as before
        -- if not more, so update the `longest' result to return:
        go (ReturnPlus o p) xs _ = go p xs (xs, Just o)

-- Like parseLongest but report final position instead of returning remaining input
parseWithPos :: P i o -> pos -> (pos -> i -> pos) -> [i] -> Either (pos, Maybe o) o
parseWithPos (P p) z nxt xs = go (p pure) xs (z, Nothing)
  where go (SymbolBind _) [] r = Left r
        go (SymbolBind k) (x:xs) (p, r) = let newPos = nxt p x in newPos `seq` go (k x) xs (newPos, r)
        go (LookBind k) s r = go (k s) s r
        go Fail xs r = Left r
        -- since we never go backwards in the input, any time we return
        -- a result, we must have used at least as much input as before
        -- if not more, so update the `longest' result to return:
        go (ReturnPlus o _) [] _ = Right o -- No input left but an output to return -> it's a success
        go (ReturnPlus o p) xs (pos, _) = go p xs (pos, Just o)

(+++) :: P i o -> P i o -> P i o
(P a) +++ (P b) = P $ \k -> a k ++++ b k

(++++) :: P' i o -> P' i o -> P' i o
Fail ++++ p = p
p ++++ Fail = p
(SymbolBind k) ++++ (SymbolBind k') = SymbolBind $ \c -> k c ++++ k' c
(LookBind k) ++++ (LookBind k') = LookBind $ \c -> k c ++++ k' c
sb@(SymbolBind _) ++++ (LookBind k) = LookBind $ \s -> sb ++++ k s
(LookBind k) ++++ sb@(SymbolBind _) = LookBind $ \s -> k s ++++ sb
(ReturnPlus a p) ++++ (ReturnPlus b q) = ReturnPlus a $ ReturnPlus b $ p ++++ q
(ReturnPlus o p) ++++ sb@(SymbolBind _) = ReturnPlus o $ p ++++ sb
(ReturnPlus o p) ++++ lb@(LookBind _) = ReturnPlus o $ p ++++ lb
sb@(SymbolBind _) ++++ (ReturnPlus o p) = ReturnPlus o $ sb ++++ p
lb@(LookBind _) ++++ (ReturnPlus o p) = ReturnPlus o $ lb ++++ p

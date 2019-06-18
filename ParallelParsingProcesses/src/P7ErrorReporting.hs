{-# LANGUAGE RankNTypes, TypeFamilies, FlexibleContexts #-}
-- Based on
-- 1. Look Ahead (Section 10 of Koen Claessen's Functional Pearl
--    Parallel Parsing Processes) and Other Parse Functions
--    (Section 11)
--    http://www.cse.chalmers.se/edu/course/course/afp/Papers/parser-claessen.pdf
-- 2. Error Reporting Parsers: a Monad Transformer Approach
--    by Matt Fenwick & Jay Vyas following along
--    https://themonadreader.files.wordpress.com/2013/08/issue221.pdf
--    pp 59--61
module P7ErrorReporting
    ( symbol
    , failp -- `fail' is already defined in Prelude -> renamed
    , P
    , parseComplete
    , parseLongest
    , parseWithPos
    , look
    , commit
    , eof
    , try
    , munch
    , munch1
    , (+++)
    , moduleName
    , MonadError(..)
    ) where

import Control.Applicative (Alternative(..))
import Data.List.NonEmpty (NonEmpty(..))

moduleName :: String
moduleName = "P7ErrorReporting"

-- Copying implementation at the end of Section 9 and will modify it

data P' e i o = Fail
              | SymbolBind (i -> P' e i o)
              | ReturnPlus (Either e o) (P' e i o) -- ReturnPlus o p === return o ++++ p
              | LookBind ([i] -> P' e i o)

instance Functor (P' e i) where
  fmap f (SymbolBind sb) = SymbolBind $ fmap f . sb
  fmap f (LookBind lb) = LookBind $ fmap f . lb
  fmap _ Fail = Fail
  fmap f (ReturnPlus o pio) = ReturnPlus (fmap f o) $ fmap f pio

instance Applicative (P' e i) where
  pure x = ReturnPlus (Right x) Fail
  Fail <*> _ = Fail
  _ <*> Fail = Fail
  SymbolBind f <*> (ReturnPlus (Left e) pio) = ReturnPlus (Left e) (SymbolBind f <*> pio)
  SymbolBind f <*> (ReturnPlus (Right o) pio) = (SymbolBind $ \i -> fmap ($ o) (f i))
                                           ++++ (SymbolBind f <*> pio)
  SymbolBind f <*> sb@(SymbolBind _) = SymbolBind $ \i -> f i <*> sb
  LookBind f <*> (ReturnPlus (Left e) pio) = ReturnPlus (Left e) (LookBind f <*> pio)
  LookBind f <*> (ReturnPlus (Right o) pio) = (LookBind $ \i -> fmap ($ o) (f i))
                                         ++++ (LookBind f <*> pio)
  LookBind f <*> lb@(LookBind _) = LookBind $ \i -> f i <*> lb
  SymbolBind f <*> lb@(LookBind _) = SymbolBind $ \i -> f i <*> lb
  LookBind f <*> sb@(SymbolBind _) = LookBind $ \i -> f i <*> sb
  (ReturnPlus f pio) <*> pa@(ReturnPlus a pjp) = ReturnPlus (f <*> a) $ rest f
    where rest (Left _) = pio <*> pa
          rest (Right f) = (fmap f pjp) ++++ (pio <*> pa)
  (ReturnPlus (Left e) pio) <*> pjp = ReturnPlus (Left e) $ (pio <*> pjp)
  (ReturnPlus (Right f) pio) <*> pjp = fmap f pjp ++++ (pio <*> pjp)

instance Monad (P' e i) where
  fail _ = Fail
  return = pure
  (SymbolBind f) >>= k = SymbolBind $ \x -> f x >>= k
  (LookBind f) >>= k = LookBind $ \x -> f x >>= k
  Fail >>= _ = Fail
  (ReturnPlus le@(Left _) p) >>= f = Fail -- ReturnPlus le $ p >>= f
  (ReturnPlus (Right o) p) >>= f = f o ++++ (p >>= f)

instance Alternative (P' e i) where
  empty = Fail
  (<|>) = (++++)

newtype P e i o = P (forall z . (o -> P' e i z) -> P' e i z)

instance Functor (P e s) where
  -- fmap :: (o -> q)
  --      -> P (forall z . (o -> P' i z) -> P' i z)
  --      -> P (forall z . (q -> P' i z) -> P' i z)
  -- -- p :: (forall z . (o -> P' i z) -> P' i z)
  -- -- k :: q -> P' i z
  fmap f (P p) = P $ \k -> p (k . f)

instance Applicative (P e s) where
  pure x = P $ \k -> k x
  p <*> q = p >>= \f -> fmap f q -- abusing Monad instance.  Paper must have been written before Applicative was a thing

symbol :: P e i i
symbol = P $ SymbolBind

failp :: P e i o
failp = P $ const Fail

errormsg :: e -> P e i o
errormsg e = P $ const $ ReturnPlus (Left e) Fail

look :: P e s [s]
look = P $ LookBind

munch :: (s -> Bool) -> P e s [s]
munch p = look >>= go
  where go (c:xs) | p c = do
                            _ <- symbol
                            fmap (c:) $ go xs
        go _ = return [] -- no input left or predicate not satisfied

munch1 :: (s -> Bool) -> P e s (NonEmpty s)
munch1 p = look >>= go1
  where go1 (c:xs) | p c = do
                             _ <- symbol
                             fmap (c :|) $ munch p

eof :: P e s ()
eof = do
        s <- look
        case s of
          [] -> return ()
          _ -> empty

-- I really don't see why `look' would be needed for this one, unless the
-- input should not be consumed.  The text is not clear enough on that for me.
try :: Alternative f => f a -> f (Maybe a)
try p = fmap Just p <|> pure Nothing

commit :: MonadError (P e i) => e -> P e i o -> P e i o
commit e p = p <|> throwError e

instance Monad (P e i) where
  return = pure
  -- P i a >>= (a -> P i b) :: P i b
  -- p :: forall z . (a -> P' i z) -> P' i z
  -- f :: a -> P (forall z . (b -> P' i z) -> P' i z)
  -- k :: b -> P' i z
  -- x :: a
  (P p) >>= f = P $ \k -> p (\x -> case f x of P p' -> p' k)
  fail = const failp

instance Alternative (P e i) where
  empty = failp
  (<|>) = (+++)

-- I guess I should actually import the `real' MonadError
class Monad m => MonadError m where
  -- | - Left zero: `throwError e >>= f = throwError e`
  -- | - Catch: `catchError (throwError e) f = f e`
  -- | - Pure: `catchError (pure a) f = pure a`
  type Error m :: *
  catchError :: m a -> (Error m -> m a) -> m a
  throwError :: Error m -> m a

instance MonadError (P e i) where
  type Error (P e i) = e
  catchError (P p) handler =
    P $ \k -> case p k of
                ReturnPlus (Left e) _ -> let (P q) = handler e in q k
                r@(ReturnPlus (Right _) _) -> r
                f@Fail -> f
                sb@(SymbolBind _) -> sb
                lb@(LookBind _) -> lb
  throwError e = P $ const $ ReturnPlus (Left e) Fail

-- These don't really make sense anymore: what does it mean to return a
-- list of partial successes (Right o) and one failure with error message
-- (Left e)?
--
-- parse :: P e i o -> [i] -> [([i], Either e o)]
-- parse' :: P' e i o -> [i] -> [([i], Either e o)]
--
-- Maybe something like `(Maybe ([i], e), [([i], o)])', i.e. a list of
-- (partial) successes and possibly the error that caused the process
-- to stop?

-- Only returns first error or first result for which the parse is complete
-- (i.e. no input is left)
--
-- The paper uses parseComplete :: P i o -> [i] -> [o] and returns all results
parseComplete :: P e i o -> [i] -> Either e (Maybe o)
parseComplete (P p) = go $ p pure
  where go (SymbolBind _) [] = Right Nothing
        go (SymbolBind k) (x:xs) = go (k x) xs
        go (LookBind k) s = go (k s) s
        go Fail _ = Right Nothing
        go (ReturnPlus (Left e) _) _ = Left e
        go (ReturnPlus (Right o) p) [] = checkThereIsNoFailure o p
        go (ReturnPlus _ p) xs = go p xs -- input not completely used up -> discard output
        -- what could happen (++++ reorders the results, there is no real ordering) is a 
        -- result like this: ReturnPlus (Right o) $ ReturnPlus (Left e) _
        checkThereIsNoFailure o (SymbolBind _) = Right $ Just o
        checkThereIsNoFailure o Fail = Right $ Just o
        checkThereIsNoFailure o (LookBind lb) = checkThereIsNoFailure o $ lb []
        checkThereIsNoFailure o (ReturnPlus (Left e) _) = Left e
        checkThereIsNoFailure o (ReturnPlus (Right _) p) = checkThereIsNoFailure o p

-- Only return (last) result using more of the input than all other results
--
-- The paper uses parseLongest :: P i o -> [i] -> Maybe (o, [i])
parseLongest :: P e i o -> [i] -> ([i], Either e (Maybe o))
parseLongest (P p) xs = go (p pure) xs (xs, Nothing)
  where go (SymbolBind _) [] r = fmap Right r -- use (,)'s Functor instance
        go (SymbolBind k) (x:xs) r = go (k x) xs r
        go (LookBind k) s r = go (k s) s r
        go Fail xs r = fmap Right r
        -- since we never go backwards in the input, any time we return
        -- a result, we must have used at least as much input as before
        -- if not more, so update the `longest' result to return:
        go (ReturnPlus (Right o) p) xs _ = go p xs (xs, Just o)
        go (ReturnPlus (Left e) _) xs _ = (xs, Left e)

-- Like parseLongest but report final position instead of returning remaining input
parseWithPos :: P e i o -> pos -> (pos -> i -> pos) -> [i] -> Either ((pos, Maybe e), Maybe o) o
parseWithPos (P p) z nxt xs = go (p pure) xs (z, Nothing)
  where go (SymbolBind _) [] pos_r = returnMuteFailure pos_r
        go (SymbolBind k) (x:xs) (p, r) = let newPos = nxt p x in newPos `seq` go (k x) xs (newPos, r)
        go (LookBind k) s pos_r = go (k s) s pos_r
        go Fail xs pos_r = returnMuteFailure pos_r
        -- since we never go backwards in the input, any time we return
        -- a result, we must have used at least as much input as before
        -- if not more, so update the `longest' result to return:
        go (ReturnPlus (Right o) p) xs (pos, _) = go p xs (pos, Just o)
        go (ReturnPlus (Left e) p) xs (pos, r) = Left ((pos, Just e), r)
        -- return a failure without an error message:
        returnMuteFailure (pos, r) = Left ((pos, Nothing), r)

(+++) :: P e i o -> P e i o -> P e i o
(P a) +++ (P b) = P $ \k -> a k ++++ b k

(++++) :: P' e i o -> P' e i o -> P' e i o
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

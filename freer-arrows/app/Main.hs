{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
module Main (main) where

import Data.Kind (Type)

class Category (cat :: k -> k -> Type) where
    ida :: forall (a :: k). cat a a
    (.>) :: forall (b :: k) (c :: k) (a :: k). cat b c -> cat a b -> cat a c

(>>>) :: forall k (a :: k) (b :: k) (c :: k) (cat :: k -> k-> Type).
             (Category cat) => cat a b -> cat b c -> cat a c
(>>>) = flip (.>)

class Category a => PreArrow a where
    arr :: (b -> c) -> a b c

class PreArrow a => Arrow a where
    first :: a b c -> a (b,d) (c, d)

instance Category (->) where
    ida = id
    (.>) = (.)

instance PreArrow (->) where
    arr = id

instance Arrow (->) where
    first f = \(b, d) -> (f b, d)

newtype AState s a b = AState { runAState :: s -> a -> (s, b) }

instance Category (AState s) where
    ida = AState { runAState = \s a -> (s, a) }
    AState f .> AState g = AState $ \s a -> let (s', a') = g s a in f s' a'

instance PreArrow (AState s) where
    arr f = AState $ \s a -> (s, f a)

instance Arrow (AState s) where
    first (AState f) = AState $ \s (a, d) -> let (s', b) = f s a in (s', (b, d))

main :: IO ()
main = putStrLn "Hello, Haskell!"

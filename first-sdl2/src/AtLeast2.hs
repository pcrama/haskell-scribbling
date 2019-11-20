{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}

module AtLeast2 (
  AtLeast2(..)
  , atLeast2
  , unCons
  )
where

import Data.List.NonEmpty (NonEmpty(..))

data AtLeast2 a = AtLeast2 { firstAL2 :: a, secondAL2 :: a, restAL2 :: [a] }
  deriving (Show, Eq, Functor, Foldable)

atLeast2 :: [a] -> Maybe (AtLeast2 a)
atLeast2 (fst2:snd2:xs) = Just $ AtLeast2 { firstAL2 = fst2, secondAL2 = snd2, restAL2 = xs }
atLeast2 _ = Nothing

unCons :: AtLeast2 a -> (a, NonEmpty a)
unCons (AtLeast2 { firstAL2 = fst2, secondAL2 = snd2, restAL2 = xs }) = (fst2, snd2 :| xs)

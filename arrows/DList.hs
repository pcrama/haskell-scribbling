module DList (
    DList
  , dList1
  , dlCons
  , dlDeconstruct
  , dlHead
  , dlNull
  , dlTail
)
where

import Data.Monoid

newtype DList a = DList ([a] -> [a])

unDList :: DList a -> [a] -> [a]
unDList (DList as) k = as k

dlCons :: a -> DList a -> DList a
dlCons a (DList as) = DList $ \k -> a:as k

dlHead :: DList a -> [a] -> Maybe a
dlHead (DList as) k = case as k of
  [] -> Nothing
  (x:_) -> Just x

dlTail :: DList a -> DList a
dlTail (DList as) = DList $ \k -> case as k of (_:xs) -> xs

dList1 :: a -> DList a
dList1 = DList . (:)

dlDeconstruct :: DList a -> Maybe (a, DList a)
dlDeconstruct (DList as) = case as [] of
                             (x:xs) -> Just (x, DList $ (++) xs)
                             [] -> Nothing

dlNull :: DList a
dlNull = DList id

instance Functor DList where
  fmap f (DList as) = DList (\k -> foldr ((:) . f) k $ as [])

instance Monoid (DList a) where
  mempty = DList id
  mappend (DList a) (DList b) = DList $ \k -> a (b k)

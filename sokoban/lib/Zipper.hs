module Zipper (
    mkZipper
  , Zipper
  , zipperFirst
  , zipperNext
  , zipperPrev
  , zipperLast
  , zipperFocus
  )

where

data Zipper a = Zipper [a] a [a]

mkZipper :: [a] -> Maybe (Zipper a)
mkZipper [] = Nothing
mkZipper (x:xs) = Just $ Zipper [] x xs

zipperPrev :: Zipper a -> Maybe (Zipper a)
zipperPrev (Zipper [] _ _) = Nothing
zipperPrev (Zipper (p:ps) f ns) = Just $ Zipper ps p $ f:ns

zipperNext :: Zipper a -> Maybe (Zipper a)
zipperNext (Zipper _ _ []) = Nothing
zipperNext (Zipper ps f (n:ns)) = Just $ Zipper (f:ps) n ns

zipperFirst :: Zipper a -> Zipper a
zipperFirst z@(Zipper [] _ _) = z
zipperFirst (Zipper (p:ps) f ns) = zipperFirst $ Zipper ps p $ f:ns

zipperLast :: Zipper a -> Zipper a
zipperLast z@(Zipper _ _ []) = z
zipperLast (Zipper ps f (n:ns)) = zipperLast $ Zipper (f:ps) n ns

zipperFocus :: Zipper a -> a
zipperFocus (Zipper _ f _) = f

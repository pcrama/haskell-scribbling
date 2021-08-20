module Tagged
( Tagged(..)
)

where

import Test.QuickCheck


newtype Tagged t a = Tagged { getTagged :: a }
  deriving (Eq, Ord, Show)


instance Num a => Num (Tagged t a) where
  (Tagged a) + (Tagged b) = Tagged $ a + b
  (Tagged a) - (Tagged b) = Tagged $ a - b
  (Tagged a) * (Tagged b) = Tagged $ a * b
  abs = via1 abs
  signum = Tagged . signum . getTagged
  fromInteger = Tagged . fromInteger


instance Integral a => Integral (Tagged t a) where
  quot = via2 quot
  rem (Tagged a) (Tagged b) = Tagged $ rem a b
  div (Tagged a) (Tagged b) = Tagged $ div a b
  mod (Tagged a) (Tagged b) = Tagged $ mod a b
  quotRem (Tagged a) (Tagged b) = (Tagged q, Tagged r)
    where (q, r) = quotRem a b
  divMod (Tagged a) (Tagged b) = (Tagged d, Tagged m)
    where (d, m) = divMod a b
  toInteger = toInteger . getTagged


instance Real a => Real (Tagged t a) where
  toRational = toRational . getTagged


via1 :: (a -> a) -> (Tagged t a -> Tagged t a)
via1 f = Tagged . f . getTagged


via2 :: (a -> a -> a) -> (Tagged t a -> Tagged t a -> Tagged t a)
via2 f (Tagged x) (Tagged y) = Tagged $ f x y


instance Fractional a => Fractional (Tagged t a) where
  (/) = via2 (/)
  recip = via1 recip
  fromRational = Tagged . fromRational


instance Enum a => Enum (Tagged t a) where
  toEnum = Tagged . toEnum
  fromEnum = fromEnum . getTagged


instance (Num a, Ord a, Arbitrary a) => Arbitrary (Tagged t a) where
  arbitrary = fmap (Tagged . getNonNegative) arbitrary
  shrink = map (Tagged . getNonNegative) . shrink . NonNegative . getTagged

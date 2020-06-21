{-# LANGUAGE FlexibleInstances #-}

module Semiring (
  HillClimber(..)
  , Semiring(..)
  , sprod
  , ssum
)

where

-- Laws:
--
-- zero `splus` x == x `splus` zero == x
-- one `stimes` x == x `stimes` one == x
-- zero `stimes` x == x `stimes` zero == zero
-- (a `splus` b) `stimes` x == (a `stimes` x) `splus` (b `stimes` x)
-- (a `splus` b) `splus` c == a `splus` (b `splus` c)
-- (a `stimes` b) `stimes` c == a `stimes` (b `stimes` c)
-- a `splus` b == b `splus` a
class Semiring s where
  zero, one :: s
  splus, stimes :: s -> s -> s

instance Semiring Bool where
  zero = False
  one = True
  splus = (||)
  stimes = (&&)

-- Num s => Semiring s would require UndecidableInstances
instance Semiring Int where
  zero = 0
  one = 1
  splus = (+)
  stimes = (*)

ssum :: (Semiring s, Foldable f) => f s -> s
ssum = foldr splus zero

sprod :: (Semiring s, Foldable f) => f s -> s
sprod = foldr stimes one

-- laws:
--
-- x `climb` x == x
-- x `climb` y == y `climb` x
-- x `climb` (y `climb` z) == (x `climb` y) `climb` z
-- x `climb` valley = x
-- x `descend` x == x
-- x `descend` y == y `descend` x
-- x `descend` (y `descend` z) == (x `descend` y) `descend` z
-- x `descend` valley = valley
class HillClimber a where
  valley :: a
  climb :: a -> a -> a
  descend :: a -> a -> a

instance HillClimber Int where
  valley = minBound
  climb x y = case compare x y of
    LT -> y
    GT -> x
    EQ -> x
  descend x y = case compare x y of
    LT -> x
    GT -> y
    EQ -> x

instance HillClimber Bool where
  valley = False
  climb = (||)
  descend = (&&)

instance (HillClimber l, Semiring r) => Semiring (Either l r) where
  zero = Left valley
  one = Right one
  splus (Left x) (Left y) = Left $ x `climb` y
  splus (Left _) (Right y) = Right y
  splus (Right x) (Left _) = Right x
  splus (Right x) (Right y) = Right $ x `splus` y
  stimes (Left x) (Left y) = Left $ x `descend` y
  stimes (Left x) (Right _) = Left x
  stimes (Right _) (Left x) = Left x
  stimes (Right x) (Right y) = Right $ x `stimes` y

instance Semiring s => Semiring (Maybe s) where
  zero = Nothing
  one = Just one
  splus Nothing x = x
  splus x Nothing = x
  splus (Just x) (Just y) = Just $ x `splus` y
  stimes Nothing _ = Nothing
  stimes _ Nothing = Nothing
  stimes (Just x) (Just y) = Just $ x `stimes` y

instance (Semiring l, Semiring r) => Semiring (l, r) where
  zero = (zero, zero)
  one = (one, one)
  splus (x, y) (a, b) = (x `splus` a, y `splus` b)
  stimes (x, y) (a, b) = (x `stimes` a, y `stimes` b)

instance (Semiring l, Semiring r, Semiring c) => Semiring (l, r, c) where
  zero = (zero, zero, zero)
  one = (one, one, one)
  splus (x, y, z) (a, b, c) = (x `splus` a, y `splus` b, z `splus` c)
  stimes (x, y, z) (a, b, c) = (x `stimes` a, y `stimes` b, z `stimes` c)

instance Semiring b => Semiring (a -> b) where
  zero = const zero
  one = const one
  splus fx fy a = fx a `splus` fy a
  stimes fx fy a = fx a `stimes` fy a

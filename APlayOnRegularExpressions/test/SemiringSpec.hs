{-# LANGUAGE TypeApplications #-}

module SemiringSpec (
  Proxy(..)
  , semiringLaws
  , semiringLawsExceptDistributivity
  , spec
  )
where

import SpecHelper
import Semiring

-- properties

data Proxy a = Proxy

prop_zeroNeutralForSplus :: (Semiring s, Eq s) => Proxy s -> s -> Bool
prop_zeroNeutralForSplus _ x = x == (zero `splus` x) && x == (x `splus` zero)

prop_oneNeutralForStimes :: (Semiring s, Eq s) => Proxy s -> s -> Bool
prop_oneNeutralForStimes _ x = x == (one `stimes` x) && x == (x `stimes` one)

prop_zeroCollapsesStimes :: (Semiring s, Eq s) => Proxy s -> s -> Bool
prop_zeroCollapsesStimes _ x = zero == (zero `stimes` x) && zero == (x `stimes` zero)

prop_stimesAssociative :: (Semiring s, Eq s) => Proxy s -> s -> s -> s -> Bool
prop_stimesAssociative _ x y z = ((x `stimes` y) `stimes` z) == (x `stimes` (y `stimes` z))

prop_splusAssociative :: (Semiring s, Eq s) => Proxy s -> s -> s -> s -> Bool
prop_splusAssociative _ x y z = ((x `splus` y) `splus` z) == (x `splus` (y `splus` z))

prop_distributive :: (Semiring s, Eq s) => Proxy s -> s -> s -> s -> Bool
prop_distributive _ x y z =
  (((x `splus` y) `stimes` z) == ((x `stimes` z) `splus` (y `stimes` z)))
  && ((z `stimes` (x `splus` y)) == ((z `stimes` x) `splus` (z `stimes` y)))

prop_splusCommutative :: (Semiring s, Eq s) => Proxy s -> s -> s -> Bool
prop_splusCommutative _ x y = (x `splus` y) == (y `splus` x)

semiringLawsExceptDistributivity :: (Arbitrary s, Show s, Semiring s, Eq s)
                                 => Proxy s -> SpecWith ()
semiringLawsExceptDistributivity p = do
  it "has zero as neutral for splus" $ property $ prop_zeroNeutralForSplus p
  it "has one as neutral for stimes" $ property $ prop_oneNeutralForStimes p
  it "zero annihilates stimes" $ property $ prop_zeroCollapsesStimes p
  it "stimes is associative" $ property $ prop_stimesAssociative p
  it "splus is associative" $ property $ prop_splusAssociative p
  it "splus is commutative" $ property $ prop_splusCommutative p

semiringLaws :: (Arbitrary s, Show s, Semiring s, Eq s)
             => Proxy s -> SpecWith ()
semiringLaws p = context "obeys the semiring laws:" $ do
  semiringLawsExceptDistributivity p
  it "distributivity" $ property $ prop_distributive p

newtype SortedNubbed a = SN [a]
  deriving (Show, Eq)

snNil :: SortedNubbed a
snNil = SN []

snCons :: Ord a => a -> SortedNubbed a -> SortedNubbed a
snCons a (SN []) = SN [a]
snCons a (SN (b:bs))
  | a < b = SN $ a:b:bs
  | a == b = SN $ b:bs
  | otherwise = let SN c = snCons a $ SN bs in SN $ b:c

instance (Arbitrary a, Ord a) => Arbitrary (SortedNubbed a) where
  arbitrary = do
    -- force type checker to recognize `as' as [a],
    -- otherwise, it is unclear which Foldable to pick.
    as <- arbitrary @[_]
    return $ foldr snCons snNil as

-- SortedNubbed [a] should be Set a
instance Ord a => HillClimber (SortedNubbed a) where
  valley = snNil -- empty set
  climb x (SN []) = x -- climb === union
  climb (SN []) y = y
  climb (SN (x:xs)) (SN (y:ys))
    | x == y = let SN z = climb (SN xs) (SN ys) in SN $ x:z
    | x < y = let SN z = climb (SN xs) (SN $ y:ys) in SN $ x:z
    | otherwise = let SN z = climb (SN $ x:xs) (SN ys) in SN $ y:z
  descend _ (SN []) = snNil -- descend == intersection
  descend (SN []) _ = snNil
  descend (SN (x:xs)) (SN y)
    | x `elem` y = let SN z = descend (SN xs) (SN y) in SN $ x:z
    | otherwise = descend (SN xs) (SN y)

spec :: Spec
spec = describe "Semiring defines" $ do
  context "a Semiring Int instance that" $
    semiringLaws (Proxy :: Proxy Int)
  context "a Semiring Bool instance that" $
    semiringLaws (Proxy :: Proxy Bool)
  context "a Maybe" $ do
    context "Int instance that" $
      semiringLaws (Proxy :: Proxy (Maybe Int))
    context "(Maybe Int) instance that" $
      semiringLaws (Proxy :: Proxy (Maybe (Maybe Int)))
  context "an Either Bool Int instance that" $ do
    semiringLaws (Proxy :: Proxy (Either Bool Int))
  context "an Either Int Int instance that" $ do
    semiringLaws (Proxy :: Proxy (Either Int Int))
  context "an Either (SortedNubbed Char) Int instance that" $ do
    semiringLaws (Proxy :: Proxy (Either (SortedNubbed Char) Int))
  context "an Either Bool (Maybe Int) instance that" $ do
    semiringLaws (Proxy :: Proxy (Either Bool (Maybe Int)))
  context "tuple Semiring instances" $ do
    semiringLaws (Proxy :: Proxy (Int, Int, Either Bool (Int, Maybe Int)))
  context "ssum that" $ do
    it "works for Bool examples" $ do
      ssum [] `shouldBe` False
      ssum Nothing `shouldBe` False
      ssum (Just True) `shouldBe` True
      ssum (Just False) `shouldBe` False
      ssum [False] `shouldBe` False
      ssum [True] `shouldBe` True
      ssum [False, True] `shouldBe` True
      ssum [False, True, False] `shouldBe` True
    it "works for Int examples" $ do
      ssum [] `shouldBe` (0 :: Int)
      ssum Nothing `shouldBe` (0 :: Int)
      ssum (Just 23) `shouldBe` (23 :: Int)
      ssum [0] `shouldBe` (0 :: Int)
      ssum [12] `shouldBe` (12 :: Int)
      ssum [0, 2] `shouldBe` (2 :: Int)
      ssum [0, 1, 0, 2] `shouldBe` (3 :: Int)
  context "sprod that" $ do
    it "works for Bool examples" $ do
      sprod [] `shouldBe` True
      sprod Nothing `shouldBe` True
      sprod (Just True) `shouldBe` True
      sprod (Just False) `shouldBe` False
      sprod [False] `shouldBe` False
      sprod [True] `shouldBe` True
      sprod [False, True] `shouldBe` False
      sprod [False, True, False] `shouldBe` False
    it "works for Int examples" $ do
      sprod [] `shouldBe` (1 :: Int)
      sprod Nothing `shouldBe` (1 :: Int)
      sprod (Just 23) `shouldBe` (23 :: Int)
      sprod [0] `shouldBe` (0 :: Int)
      sprod [12] `shouldBe` (12 :: Int)
      sprod [0, 2] `shouldBe` (0 :: Int)
      sprod [0, 1, 0, 2] `shouldBe` (0 :: Int)
      sprod [1, 2, 3, 4, 5] `shouldBe` (120 :: Int)
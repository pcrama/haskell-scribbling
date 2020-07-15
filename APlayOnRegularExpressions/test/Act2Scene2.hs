module Act2Scene2 (
  spec
)
where

import Semiring
import SemiringSpec (
  Proxy(..)
  , semiringLaws
  , semiringLawsExceptDistributivity
  )
import SpecHelper
import LibAct2

testSubmatchW :: (Show a, Eq a, Show s, Eq s, Semiring s)
              => Reg s (Int, a) -> [a] -> s -> SpecWith ()
testSubmatchW r as expected =
  it ((if (expected == zero) then "rejec" else "accep") ++ "ts " ++ show as) $
    submatchW r as `shouldBe` expected

newtype ArbLeftmostMatch = ALM LeftmostMatch
  deriving (Show, Eq)

instance Arbitrary ArbLeftmostMatch where
  arbitrary = do
    NonNegative value <- arbitrary
    ALM <$> oneof [return NoLeft, return $ Leftmost NoStart, return $ Leftmost $ Start value]
  shrink (ALM NoLeft) = []
  shrink (ALM (Leftmost s)) = [ALM NoLeft] ++ case s of
                                              NoStart -> []
                                              Start i -> [ALM $ Leftmost NoStart]
                                                      ++ [ALM $ Leftmost $ Start x | x <- shrink i]

instance Semiring ArbLeftmostMatch where
  zero = ALM zero
  one = ALM one
  (ALM x) `splus` (ALM y) = ALM $ x `splus` y
  (ALM x) `stimes` (ALM y) = ALM $ x `stimes` y

newtype ArbLeftLongMatch = ALLM LeftLong
  deriving (Show, Eq)

instance Arbitrary ArbLeftLongMatch where
  arbitrary = do
    NonNegative a <- arbitrary
    NonNegative b <- arbitrary
    ALLM <$> oneof [return NoLeftLong, return $ LeftLong NoRange, return $ LeftLong $ Range a $ a + b]
  shrink (ALLM NoLeftLong) = []
  shrink (ALLM (LeftLong s)) = [ALLM NoLeftLong]
                            ++ case s of
                                 NoRange -> []
                                 Range i j -> [ALLM $ LeftLong NoRange]
                                           ++ if (i < j) then [ALLM $ LeftLong $ Range x x | x <- shrink i] else []

instance Semiring ArbLeftLongMatch where
  zero = ALLM zero
  one = ALLM one
  (ALLM x) `splus` (ALLM y) = ALLM $ x `splus` y
  (ALLM x) `stimes` (ALLM y) = ALLM $ x `stimes` y

prop_adHocDistributiveLeftNoRange :: ArbLeftLongMatch -> ArbLeftLongMatch -> Bool
prop_adHocDistributiveLeftNoRange x y =
  let noRange = ALLM $ LeftLong NoRange in
    noRange `stimes` (x `splus` y) == (noRange `stimes` x) `splus` (noRange `stimes` y)

prop_adHocDistributiveRightNoRange :: ArbLeftLongMatch -> ArbLeftLongMatch -> Bool
prop_adHocDistributiveRightNoRange x y =
  let noRange = ALLM $ LeftLong NoRange in
    (x `splus` y) `stimes` noRange == (x `stimes` noRange) `splus` (y `stimes` noRange)

-- NB: this is slightly more general than the real usage where x == y == Range _ _,
-- but as long as the property holds for the general case, it holds for the specific
-- case.
prop_adHocDistributiveNoLeftLongInSum :: ArbLeftLongMatch -> ArbLeftLongMatch -> Bool
prop_adHocDistributiveNoLeftLongInSum x y =
  let noLL = ALLM $ NoLeftLong in
    ((x `splus` y) `stimes` noLL == (x `stimes` noLL) `splus` (y `stimes` noLL)) && (
      (noLL `stimes` (x `splus` y)) == ((noLL `stimes` x) `splus` (noLL `stimes` y)))

prop_adHocDistributiveWithRange :: NonNegative Int -> NonNegative Int -> NonNegative Int -> NonNegative Int -> Bool
prop_adHocDistributiveWithRange (NonNegative i) (NonNegative b) (NonNegative c) (NonNegative d) =
  (let j = i + (max b c)
       k = i + (min b c)
       p = j + 1
       q = p + d
       x = ALLM $ LeftLong $ Range i j
       y = ALLM $ LeftLong $ Range k j
       z = ALLM $ LeftLong $ Range p q in
    ((x `splus` y) `stimes` z == (x `stimes` z) `splus` (y `stimes` z))) && (
   let j = i + b
       k = j + 1
       p = k + c
       q = k + d
       z = ALLM $ LeftLong $ Range i j
       x = ALLM $ LeftLong $ Range k p
       y = ALLM $ LeftLong $ Range k q in
    ((z `stimes` (x `splus` y)) == ((z `stimes` x) `splus` (z `stimes` y))))

-- The functional pearl provides an implementation of splus and stimes for
-- LeftLong.  This implementation does not meet the semiring laws when picking
-- arbitrary combinations of values (see some counterexamples found by
-- QuickCheck in comment below this spec for the proposed implementation and
-- various attempts at fixing).  The failures in the proposed implementation
-- all concern distributivity.  Some of the attempts violated associativity of
-- stimes: those counterexamples are included, too.
--
-- However, the operations with LeftLong are used in the specific context of
-- matching a regular expression.  Hence, instead of using the more general
-- semiring distributivity law, we restrict the values to match the expected
-- usage:
adHocSemiringLaws :: SpecWith ()
adHocSemiringLaws = context "obeys the semiring laws when used responsibly:" $ do
  semiringLawsExceptDistributivity (Proxy :: Proxy ArbLeftLongMatch)
  context "ad-hoc distributivity" $ do
    -- LeftLong NoRange can occur to the left of the sum (i.e. the arbitrary
    -- part of the unanchored match before the match we are actually looking
    -- for):
    it "with left (LeftLong NoRange)" $ property $ prop_adHocDistributiveLeftNoRange
    -- LeftLong NoRange can occur to the right of the sum (i.e. the arbitrary
    -- part of the unanchored match after the match we are actually looking
    -- for):
    it "with right (LeftLong NoRange)" $ property $ prop_adHocDistributiveRightNoRange
    -- Then, there are cases where LeftLong NoRange is not involved:
    -- (sub)expressions inside the match we are looking for.  There, the
    -- values are either NoLeftLong or (LeftLong $ Range a b).  Also, there
    -- are relations between the indices when several (LeftLong $ Range a b)
    -- are involved because in (x `splus` y) `stimes` z, x and y correspond to
    -- the same range (if they match) and z must follow x and/or y without a
    -- gap:
    context "for NoLeftLong" $ do
      it "all" $ do
        (NoLeftLong `stimes` (NoLeftLong `splus` NoLeftLong)) `shouldBe` (
          (NoLeftLong `stimes` NoLeftLong) `splus` (NoLeftLong `stimes` NoLeftLong))
        ((NoLeftLong `splus` NoLeftLong) `stimes` NoLeftLong) `shouldBe` (
          (NoLeftLong `stimes` NoLeftLong) `splus` (NoLeftLong `stimes` NoLeftLong))
      it "inside sum" $ property prop_adHocDistributiveNoLeftLongInSum
    it "for Range _ _" $
      property prop_adHocDistributiveWithRange
-- Counterexamples for general semiring laws for proposed implementations and fixing attempts
--
-- context "respects the semiring laws (examples previously found by quickcheck)" $ do
--   context "distributivity" $
--     let distributivityCounterexample s x y z = it s $ do
--           (((LeftLong x) `splus` (LeftLong y)) `stimes` (LeftLong z)) `shouldBe` (
--               ((LeftLong x) `stimes` (LeftLong z)) `splus` ((LeftLong y) `stimes` (LeftLong z)))
--           ((LeftLong z) `stimes` ((LeftLong x) `splus` (LeftLong y))) `shouldBe` (
--               ((LeftLong z) `stimes` (LeftLong x)) `splus` ((LeftLong z) `stimes` (LeftLong y))) in do
--       distributivityCounterexample "ex 1" (Range 13 13) (Range 27 47) (Range 11 44)
--       distributivityCounterexample "ex 2" (Range 88 127) (Range 16 16) (Range 3 95)
--       distributivityCounterexample "ex 3" (Range 50 91) (Range 5 5) (Range 1 50)
--       distributivityCounterexample "ex 4" (Range 14 14) (Range 0 0) (Range 0 0)
--       distributivityCounterexample "ex 5" (Range 0 0) NoRange (Range 2 2)
--       distributivityCounterexample "ex 6" NoRange (Range 0 0) (Range 2 11)
--       distributivityCounterexample "ex 7" NoRange (Range 47 47) (Range 0 0)
--   context "associativity of stimes" $
--     let associativityCounterexample s x y z = it s $
--           (((LeftLong x) `stimes` (LeftLong y)) `stimes` (LeftLong z)) `shouldBe` (
--               (LeftLong x) `stimes` ((LeftLong y) `stimes` (LeftLong z))) in do
--       associativityCounterexample "ex 1" (Range 5 14) (Range 9 9) (Range 11 26)
--       associativityCounterexample "ex 2" (Range 49 115) (Range 16 51) (Range 64 144)
--       associativityCounterexample "ex 3" (Range 31 38) (Range 4 32) (Range 36 44)
--       associativityCounterexample "ex 4" (Range 0 0) (Range 0 0) (Range 10 10)

spec :: Spec
spec = describe "A Play on Regular Expressions, Act 2 Scene 2" $ do
  context "has a function submatchW that" $ do
    context "works on an example" $
      let test = testSubmatchW (symS $ \(i, c) -> case i `mod` 2 of
                                                    0 -> c == '0'
                                                    1 -> c == '1'
                                                    _ -> error "new math: `mod` 2 returned neither 0, nor 1" ) in do
        test "" False
        test "0" True
        test "1" False
        test "abc" False
        test "a01bc" False
        test "a00a" True
    context "finds leftmost match" $
      let test = testSubmatchW (seqS (symI 'a') $ symI 'b') in do
        test "" NoLeft
        test "a" NoLeft
        test "ab" $ Leftmost $ Start 0
        test "0abab" $ Leftmost $ Start 1
        test "012ab" $ Leftmost $ Start 3
    context "finds range of leftmost match" $
      let a = symI 'a'
          b = symI 'b'
          c = symI 'c'
          aabbcc = foldr (seqS . symI) epsS "aabbcc"
          repA = repS a
          repB = repS b
          repC = repS c
          test = testSubmatchW $ seqS a $ altS (seqS repA $ seqS repB repC) aabbcc in do
        test "" NoLeftLong
        test "zzzaabbcczzz" $ LeftLong $ Range 3 8
        test "zzzaabbccczzz" $ LeftLong $ Range 3 9
  context "defines a type LeftmostMatch that" $
    semiringLaws (Proxy :: Proxy ArbLeftmostMatch)
  context "defines a type LeftLong that" $ do
    adHocSemiringLaws

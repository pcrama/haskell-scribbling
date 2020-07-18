module SpecHelper (
  module Test.QuickCheck
, module Test.Hspec
, spec_matchSameWithAndWithoutCaching
) where

import Test.QuickCheck
import Test.Hspec

import LibAct2 (
  RegMX(..)
  , matchMX
  , split2
  , sym
  )

newtype ArbRegMX = ARX (RegMX Char)
  deriving Show

instance Arbitrary ArbRegMX where
  arbitrary = ARX <$> sized arb'
    where arb' n | n <= 0 = return EpsMX
                 | otherwise = oneof [
            return $ sym 'a',
            return $ sym 'b',
            return $ sym 'c',
            AltMX <$> (arb' $ n `div` 2) <*> (arb' $ n `div` 2),
            SeqMX <$> (arb' $ n `div` 2) <*> (arb' $ n `div` 2),
            RepMX <$> (arb' $ n `div` 2)]
  shrink (ARX EpsMX) = []
  shrink (ARX (SymMX _ c)) = [ARX EpsMX] ++ if (c == 'a') then [] else [ARX $ sym 'a']
  shrink (ARX (AltMX a b)) = [ARX $ sym 'b', ARX a, ARX b]
                          ++ [ARX $ AltMX a' b'
                             | ARX a' <- (shrink $ ARX a)
                             , ARX b' <- (shrink $ ARX b)
                             , isNotEpsMXEpsMX a' b']
                          ++ [ARX $ AltMX a' b | ARX a' <- (shrink $ ARX a), isNotEpsMXEpsMX a' b]
                          ++ [ARX $ AltMX a b' | ARX b' <- (shrink $ ARX b), isNotEpsMXEpsMX a b']
    where isNotEpsMXEpsMX EpsMX EpsMX = False
          isNotEpsMXEpsMX _ _ = True
  shrink (ARX (SeqMX a b)) = [ARX $ sym 'c', ARX a, ARX b]
                          ++ [ARX $ SeqMX a' b'
                             | ARX a' <- (shrink $ ARX a)
                             , ARX b' <- (shrink $ ARX b)
                             , isNotEpsMXEpsMX a' b']
                          ++ [ARX $ SeqMX a' b | ARX a' <- (shrink $ ARX a), isNotEpsMXEpsMX a' b]
                          ++ [ARX $ SeqMX a b' | ARX b' <- (shrink $ ARX b), isNotEpsMXEpsMX a b']
    where isNotEpsMXEpsMX EpsMX EpsMX = False
          isNotEpsMXEpsMX _ _ = True
  shrink (ARX (RepMX a)) = [ARX $ sym 'b', ARX a]
                          ++ [ARX $ RepMX a'
                             | ARX a' <- (shrink $ ARX a)
                             , case a' of
                                 EpsMX -> False
                                 _ -> True]

newtype ArbInput = AIn String
  deriving Show

instance Arbitrary ArbInput where
  arbitrary = AIn <$> sized arb'
    where arb' n = traverse (oneof . map return) $ replicate n "abc012"
  shrink (AIn []) = []
  shrink (AIn s@(c:t)) = map AIn $
    let (l, r) = split2 s
        (ll, lr) = split2 l
        (rl, rr) = split2 r
        simplify exclude x
          | x `elem` exclude = 'a'
          | x `elem` "abc" = x
          | otherwise = '0'
        keepnew x = (not $ null x) && (x /= s) in
     "":(filter keepnew [
           [c],
           ll, lr, rl, rr,
           l, r,
           t,
           map (simplify "bc") s,
           map (simplify "c") s,
           map (simplify "b") s])

newtype MatchingRegAndInput = MRAI (ArbRegMX, ArbInput)
  deriving (Show)

instance Arbitrary MatchingRegAndInput where
  arbitrary = do
      ARX rx <- arbitrary
      s <- genInput rx
      return $ MRAI (ARX rx, AIn s)
    where genInput EpsMX = return []
          genInput (SymMX _ c) = return [c]
          genInput (AltMX x y) = do
            r <- oneof [return x, return y]
            genInput r
          genInput (SeqMX x y) = do
            xin <- genInput x
            yin <- genInput y
            return $ xin ++ yin
          genInput (RepMX r) = do
            NonNegative c <- arbitrary
            fmap concat $ traverse genInput $ replicate (c `mod` 10) r

prop_cachingNonMatchIsEquivalentToNormalNonMatch :: (RegMX Char -> String -> Bool) -> ArbRegMX -> ArbInput -> Property
prop_cachingNonMatchIsEquivalentToNormalNonMatch f (ARX r) (AIn s) =
  let found = matchMX r s
      nonTrivial EpsMX = False
      nonTrivial (SymMX _ _) = True
      nonTrivial (AltMX x y) = nonTrivial x || nonTrivial y
      nonTrivial (SeqMX x y) = nonTrivial x || nonTrivial y
      nonTrivial (RepMX x) = nonTrivial x in
    checkCoverage $
    cover 80 (not found) "no match" $
    cover 80 (nonTrivial r) "non-trivial" $
    property $
    found == f r s

prop_cachingMatchIsEquivalentToNormalMatch :: (RegMX Char -> String -> Bool) -> MatchingRegAndInput -> Property
prop_cachingMatchIsEquivalentToNormalMatch f (MRAI (ARX r, AIn s)) =
  let nonTrivial EpsMX = False
      nonTrivial (SymMX _ _) = True
      nonTrivial (AltMX x y) = nonTrivial x || nonTrivial y
      nonTrivial (SeqMX x y) = nonTrivial x || nonTrivial y
      nonTrivial (RepMX x) = nonTrivial x in
    checkCoverage $
    cover 80 (nonTrivial r) "non-trivial" $
    property $
    matchMX r s && f r s

spec_matchSameWithAndWithoutCaching :: (RegMX Char-> String -> Bool) -> SpecWith ()
spec_matchSameWithAndWithoutCaching f = context "matches the same with and without caching" $ do
  it "match" $ property $ prop_cachingMatchIsEquivalentToNormalMatch f
  it "non-match" $ property $ prop_cachingNonMatchIsEquivalentToNormalNonMatch f

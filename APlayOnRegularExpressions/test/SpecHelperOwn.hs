-- copy paste of SpecHelper to get spec_matchSameWithAndWithoutCaching for LibOwn
module SpecHelperOwn (
  module Test.QuickCheck
, module Test.Hspec
, spec_matchSameWithAndWithoutCaching
) where

import Data.Char (isLetter, isDigit)
import Test.QuickCheck
import Test.Hspec

import LibOwn (
  RegMX(..)
  , matchMX
  , sym
  )

import LibAct2 (
  split2
  )

newtype ArbRegMX = ARX (RegMX Char)
  deriving Show

letterThenDigitPre :: Maybe Char -> Maybe Char -> Bool
letterThenDigitPre _ Nothing = False
letterThenDigitPre Nothing (Just d) = isDigit d
letterThenDigitPre (Just c) (Just d) = isLetter c && isDigit d

letterThenDigitPost :: Maybe Char -> Maybe Char -> Bool
letterThenDigitPost Nothing _ = False
letterThenDigitPost (Just c) Nothing = isLetter c
letterThenDigitPost (Just c) (Just d) = isLetter c && isDigit d

startWithDigit :: [Char] -> Bool
startWithDigit [] = False
startWithDigit (c:_) = isDigit c

endWithLetter :: [Char] -> Bool
endWithLetter [] = False
endWithLetter xs = isLetter $ last xs

instance Arbitrary ArbRegMX where
  arbitrary = ARX <$> sized arb'
    where arb' n | n <= 0 = return EpsMX
                 | otherwise = oneof [
            return $ sym 'a',
            return $ sym 'b',
            return $ sym '0',
            return $ sym '1',
            PreMX Nothing letterThenDigitPre <$> (arb' $ n - 1),
            (\r -> PostMX r Nothing letterThenDigitPost) <$> (arb' $ n - 1),
            AltMX <$> (arb' $ n `div` 2) <*> (arb' $ n `div` 2),
            SeqMX <$> (arb' $ n `div` 2) <*> (arb' $ n `div` 2),
            RepMX <$> (arb' $ n `div` 2)]
  shrink (ARX EpsMX) = []
  shrink (ARX (SymMX _ c)) = [ARX EpsMX] ++ if (c == 'a') then [] else [ARX $ sym 'a']
  shrink (ARX (PreMX _ _ EpsMX)) = []
  shrink (ARX (PreMX p f r)) = map ARX $ r:(map (\(ARX s) -> PreMX p f s) $ shrink $ ARX r)
  shrink (ARX (PostMX EpsMX _ _)) = []
  shrink (ARX (PostMX r p f)) = map ARX $ r:(map (\(ARX s) -> PostMX s p f) $ shrink $ ARX r)
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
          genInput (PreMX _ _ r) = do
            attempts <- traverse genInput (replicate 10 r)
            case filter startWithDigit attempts of
              [] -> return []
              probablyValid -> oneof $ map return probablyValid
          genInput (PostMX r _ _) = do
            attempts <- traverse genInput (replicate 10 r)
            case filter endWithLetter attempts of
              [] -> return []
              probablyValid -> oneof $ map return probablyValid
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
  shrink (MRAI (arMX, aIn)) =
    [MRAI (r, i)
    | r@(ARX rx) <- shrink arMX
    , i@(AIn i_) <- shrink aIn
    , matchMX rx i_]

nonTrivialMX :: RegMX Char -> Bool
nonTrivialMX EpsMX = False
nonTrivialMX (SymMX _ _) = True
nonTrivialMX (PreMX _ _ r) = nonTrivialMX r
nonTrivialMX (PostMX r _ _) = nonTrivialMX r
nonTrivialMX (AltMX x y) = nonTrivialMX x || nonTrivialMX y
nonTrivialMX (SeqMX x y) = nonTrivialMX x || nonTrivialMX y
nonTrivialMX (RepMX x) = nonTrivialMX x

prop_cachingNonMatchIsEquivalentToNormalNonMatch :: (RegMX Char -> r)
                                                 -> (String -> s)
                                                 -> (r -> s -> Bool)
                                                 -> ArbRegMX
                                                 -> ArbInput
                                                 -> Property
prop_cachingNonMatchIsEquivalentToNormalNonMatch compile pack f (ARX r) (AIn s) =
  let found = matchMX r s in
    checkCoverage $
    cover 70 (not found) "no match" $
    cover 70 (nonTrivialMX r) "non-trivial" $
    cover 50 (not $ null s) "input non-empty" $
    property $
    found == f (compile r) (pack s)

prop_cachingMatchIsEquivalentToNormalMatch :: (RegMX Char -> r)
                                           -> (String -> s)
                                           -> (r -> s -> Bool)
                                           -> MatchingRegAndInput
                                           -> Property
prop_cachingMatchIsEquivalentToNormalMatch compile pack f (MRAI (ARX r, AIn s)) =
  let found = matchMX r s in
    checkCoverage $
    cover 70 found "match" $
    cover 70 (nonTrivialMX r) "non-trivial" $
    cover 50 (not $ null s) "input non-empty" $
    property $
    found == f (compile r) (pack s)

spec_matchSameWithAndWithoutCaching :: (RegMX Char -> r)
                                    -> (String -> s)
                                    -> (r -> s -> Bool)
                                    -> String
                                    -> SpecWith ()
spec_matchSameWithAndWithoutCaching compile pack f s = context (s ++ " matches the same with and without caching") $ do
  it "match" $ property $ prop_cachingMatchIsEquivalentToNormalMatch compile pack f
  it "non-match" $ property $ prop_cachingNonMatchIsEquivalentToNormalNonMatch compile pack f

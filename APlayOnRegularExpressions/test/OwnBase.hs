module OwnBase (
  spec
)
where

import Data.Char (isAlphaNum)
import qualified Data.Text as T

import LibOwn
import Semiring (Semiring(..))
import SpecHelperOwn

testMatchMX :: (Show a, Eq a)
            => RegMX a -> [a] -> Bool -> SpecWith ()
testMatchMX r as expected =
  it ((if expected then "accep" else "rejec") ++ "ts " ++ show as) $
     matchMX r as `shouldBe` expected

testAll :: Monad m => (x -> y -> m a) -> [(x, y)] -> m ()
testAll f = foldr g (return ())
  where g (x, y) m = f x y >> m

testMatchS :: (Show a, Eq a, Show s, Eq s, Semiring s)
            => Reg s a -> [a] -> s -> SpecWith ()
testMatchS r as expected =
  it ((if (expected == zero) then "rejec" else "accep") ++ "ts " ++ show as) $
     matchS r as `shouldBe` expected

testMatchSTwice :: (Show s, Eq s, Semiring s)
                => Reg s Char -> [Char] -> s -> SpecWith ()
testMatchSTwice r as expected =
  let verb s = (if (expected == zero) then "rejec" else "accep") ++ "ts " ++ show as ++ " (" ++ s ++ ")" in do
    it (verb "String") $ matchS r as `shouldBe` expected
    it (verb "Text") $ tmatchS r (T.pack as) `shouldBe` expected

symC :: Semiring s => Char -> Reg s Char
symC c = symS $ \x -> if x == c then one else zero

startOfWordThenS :: Semiring s => Reg s Char -> Reg s Char
startOfWordThenS = preS Nothing f
  where f Nothing (Just c) = boolToSemiring $ isAlphaNum c
        f (Just b) (Just c) = boolToSemiring $ (not $ isAlphaNum b) && isAlphaNum c
        f _ Nothing = zero

endOfWordAfterS :: Semiring s => Reg s Char -> Reg s Char
endOfWordAfterS r = postS r f Nothing Nothing
  where f Nothing _ = zero
        f (Just c) Nothing = boolToSemiring $ isAlphaNum c
        f (Just c) (Just d) = boolToSemiring $ isAlphaNum c && (not $ isAlphaNum d)

spec :: Spec
spec = describe "Own extension (base implementation)" $ do
  context "has a function matchMX that matches text for" $ do
    context "basic operators:" $ do
      context "EpsMX" $ let test = testMatchMX EpsMX in do
        test [] True
        test "a" False
        test "ab" False
      context "SymMX _ 'a'" $ let test = testMatchMX $ sym 'a' in do
        test "" False
        test "a" True
        test "b" False
        test "ab" False
      context "PreMX _ _ _" $ do
        context "empty input" $ let f Nothing Nothing = True
                                    f _ _ = False
                                    g x y = not $ f x y
                                    testPos = testMatchMX $ PreMX Nothing f EpsMX
                                    testNeg = testMatchMX $ PreMX Nothing g EpsMX in do
          testPos "" True
          testPos "a" False
          testNeg "" False
          testNeg "a" False
        context "one char" $ let test = testMatchMX $ startOfWordThen $ sym 'a' in do
          test "" False
          test "a" True
          test "1" False
          test "-" False
          test "-a" False
          test "a-" False
        context "two chars -\\<b" $ let test = testMatchMX $ SeqMX (sym '-') $ startOfWordThen $ sym 'b' in do
          test "" False
          test "-" False
          test "-a" False
          test "-b" True
          test "b-" False
      context "PostMX _ _ _" $ do
        context "empty input" $ let f Nothing Nothing = True
                                    f _ _ = False
                                    g x y = not $ f x y
                                    testPos = testMatchMX $ PostMX EpsMX Nothing f
                                    testNeg = testMatchMX $ PostMX EpsMX Nothing g in do
          testPos "" True
          testPos "a" False
          testNeg "" False
          testNeg "a" False
        context "one char" $ let test = testMatchMX $ endOfWordAfter $ sym 'a' in do
          test "" False
          test "a" True
          test "1" False
          test "-" False
          test "-a" False
          test "a-" False
        context "two chars [ab]\\>[a-]" $ let test = testMatchMX $ (endOfWordAfter $ sym 'a' `AltMX` sym 'b') `SeqMX` (sym 'b' `AltMX` sym '-') in do
          test "" False
          test "-" False
          test "-a" False
          test "-b" False
          test "b-" True
          test "aa" False
      context "SeqMX ['a'] ['b']" $ let a = sym 'a'
                                        b = sym 'b'
                                        s = SeqMX a b
                                        test = testMatchMX s in do
        test "" False
        test "a" False
        test "b" False
        test "ab" True
        test "abc" False
        test "cab" False
      context "AltMX (sym 'a') (sequ \"abc\")" $
        let test = testMatchMX $ AltMX (sym 'a') (sequ "abc") in do
          test "" False
          test "a" True
          test "ab" False
          test "aba" False
          test "abc" True
          test "abca" False
      context "RepMX $ AltMX (sym 'a') $ sequ \"abc\"" $
        let test = testMatchMX $ RepMX $ AltMX (sym 'a') $ sequ "abc" in do
          test "" True
          test "a" True
          test "ab" False
          test "aba" False
          test "abc" True
          test "abca" True
          test "abcab" False
          test "abcabc" True
          test "abcabca" True
          test "abcaabca" True
      context "SeqMX (RepMX $ AltMX EpsMX $ sym 'a') $ sequ \"aaaaaaaaaa\"" $
        let test = testMatchMX $ SeqMX (RepMX $ AltMX EpsMX $ sym 'a') $ sequ "aaaaaaaaaa" in do
          flip mapM_ [0..20] $ \len ->
            test (replicate len 'a') $ len >= 10
    context "composite examples" $
      let a = sym 'a'
          b = sym 'b'
          c = sym 'c'
          m = sym '@' in do
        -- (a|b|@)*(\<a|b)c
        context "(RepMX $ a `AltMX` b `AltMX` @) `SeqMX` ((startOfWord a) `AltMX` b) `SeqMX` c" $
          testAll (testMatchMX $ (RepMX $ a `AltMX` b `AltMX` m) `SeqMX` ((startOfWordThen a) `AltMX` b) `SeqMX` c)
                  [("", False), ("a", False), ("ac", True),
                   ("bc", True), ("a@bbc", True), ("a@bac", False),
                   ("a@b@ac", True), ("a@b@aca", False)]
        -- (a\>)*[@b]
        context "(RepMX $ endOfWordAfter a) `SeqMX` (@ `AltMX` b)" $
          testAll (testMatchMX $ (RepMX $ endOfWordAfter a) `SeqMX` (m `AltMX` b))
                  [("", False), ("@", True), ("b", True), ("a@", True)
                  , ("aa@", False), ("aaa@", False), ("aaaa@", False)
                  , ("@b", False), ("ab", False)]
        -- a\>b@
        context "endOfWordAfter a `SeqMX` b `SeqMX` @" $
          testAll (testMatchMX $ endOfWordAfter a `SeqMX` b `SeqMX` m)
                  [("ab@", False), ("a", False), ("ab", False), ("a@", False)]
        context "(\\<ab|@|ba\\>|\\<aa\\>)*(ab@)" $
          let abmbaaa = RepMX $ (startOfWordThen $ sequ "ab")
                                `AltMX` m
                                `AltMX` (endOfWordAfter $ sequ "ba")
                                `AltMX` (startOfWordThen $ endOfWordAfter $ sequ "aa")
              abm = a `AltMX` b `AltMX` m in
            testAll (testMatchMX $ abmbaaa `SeqMX` abm)
                    [("", False), ("a", True), ("b", True), ("@", True)
                    , ("@@@@@", True), ("ab@ab@a", True), ("abbaab@", False)
                    , ("abba@aba", True), ("aaa", False)]
  context "has a function matchS that matches text for" $ do
    context "basic operators:" $ do
      context "epsS" $ let test = testMatchSTwice epsS in do
        test [] True
        test "a" False
        test "ab" False
      context "symS" $ let testC = testMatchSTwice (symC 'a')
                           testOdd = testMatchS $ symS (`mod` (2 :: Int)) in do
        testC "" (0 :: Int)
        testC "a" 1
        testC "b" 0
        testC "ab" 0
        testOdd [] 0
        testOdd [1] 1
        testOdd [2] 0
        testOdd [3] 1
        testOdd [1, 3] 0
      context "altS" $ let test = testMatchSTwice $ altS (symC 'a') (symC 'b') in do
        test "" False
        test "a" True
        test "b" True
        test "c" False
        test "ab" False
        test "aa" False
        test "cc" False
      context "seqS" $ let test = testMatchSTwice $ seqS (symC 'a') (symC 'b') in do
        test [] False
        test "a" False
        test "b" False
        test "c" False
        test "ab" True
        test "aa" False
        test "aba" False
      context "repS" $ let test = testMatchS $ repS $ symS (`mod` (3 :: Int)) in do
        test [] 1
        test [0] 0
        test [0, 1] 0
        test [1] 1
        test [1, 2] 2
        test [1, 2, 3] 0
        test [1, 2, 4] 2
        test [1, 2, 4, 5] 4
        test [2, 5, 8, 11] 16
      context "PreS _ _ _" $ do
        context "empty input" $ let f Nothing Nothing = True
                                    f _ _ = False
                                    g x y = not $ f x y
                                    testPos = testMatchSTwice $ postS epsS f Nothing Nothing
                                    testNeg = testMatchSTwice $ postS epsS g Nothing Nothing in do
          testPos "" True
          testPos "a" False
          testNeg "" False
          testNeg "a" False
        context "one char" $ let test = testMatchSTwice $ startOfWordThenS $ symC 'a' in do
          test "" False
          test "a" True
          test "1" False
          test "-" False
          test "-a" False
          test "a-" False
        context "two chars -\\<b" $ let test = testMatchSTwice $ seqS (symC '-') $ startOfWordThenS $ symC 'b' in do
          test "" False
          test "-" False
          test "-a" False
          test "-b" True
          test "b-" False
      context "PostS _ _ _" $ do
        context "empty input" $ let f Nothing Nothing = True
                                    f _ _ = False
                                    g x y = not $ f x y
                                    testPos = testMatchSTwice $ postS epsS f Nothing Nothing
                                    testNeg = testMatchSTwice $ postS epsS g Nothing Nothing in do
          testPos "" True
          testPos "a" False
          testNeg "" False
          testNeg "a" False
        context "one char" $ let test = testMatchSTwice $ endOfWordAfterS $ symC 'a' in do
          test "" False
          test "a" True
          test "1" False
          test "-" False
          test "-a" False
          test "a-" False
        context "two chars [ab]\\>[a-]" $ let test = testMatchSTwice $ (endOfWordAfterS $ symC 'a' `altS` symC 'b') `seqS` (symC 'b' `altS` symC '-') in do
          test "" False
          test "-" False
          test "-a" False
          test "-b" False
          test "b-" True
          test "aa" False
    context "more complex regular expressions" $ do
      context "eps*" $ do
        let test = testMatchSTwice $ repS epsS in do
          test "" True
          test "a" False
          test "bc" False
      let a = symC 'a'
          aOrEps = altS a epsS
          a2 = seqS a a
          a4 = seqS a2 a2
          a8 = seqS a4 a4
          a10 = seqS a8 a2 in do
        context "(a|eps)" $
          let test = testMatchSTwice aOrEps in do
            test "" True
            test "a" True
            test "b" False
            test "aa" False
        context "(a|eps)*" $
          let test = testMatchSTwice $ repS aOrEps in do
            test "" True
            test "a" True
            test "b" False
            test "aa" True
            test "ba" False
            test "ab" False
        context "a{4}" $
          let test = testMatchSTwice $ a4 in do
            flip mapM_ [0..6] $ \len -> do
              test (replicate len 'a') $ len == 4
              test ('b':(replicate len 'a')) False
              test ((replicate len 'a') ++ "b") False
        context "(a|<eps>)*a{10}" $
          let test = testMatchSTwice $ seqS (repS aOrEps) a10 in do
            flip mapM_ [0..20] $ \len -> do
              test (replicate len 'a') $ len >= 10
              test ('b':(replicate len 'a')) False
              test ((replicate len 'a') ++ "b") False
            test "aaaaaaaaaab" False
            test "baaaaaaaaaa" False
      context "a*b(a|b){16}b(ab|ba)*" $
        let a = symC 'a'
            b = symC 'b'
            ab = seqS a b
            ba = seqS b a
            aOrB = altS a b
            abOrBa = altS ab ba
            aOrB2 = seqS aOrB aOrB
            aOrB4 = seqS aOrB2 aOrB2
            aOrB8 = seqS aOrB4 aOrB4
            aOrB16 = seqS aOrB8 aOrB8
            test = testMatchSTwice $ seqS (seqS (repS a) b) $ seqS aOrB16 $ seqS b $ repS abOrBa in do
          test "" False
          test "bb" False
          test "bbbbbbbbbbbbbbbbbb" True
          test "abbbbbbbbbbbbbbbbbb" True
          test "bbbbbbbbbbbbbbbbbba" False
          test "bbbbbbbbbbbbbbbbbbab" True
          test "bbbbbbbbbbbbbbbbbbba" True
          test "bbbbbbbbbbbbbbbbbbbaabababbabaababba" True
          test "bbbbbbbbbbbbbbbbbbbaabababbabaababbab" False
          test "bbbbbbbbabbbbbbbbbbaabababbabaababbab" False
          test "aaabbbbbbbbabbbbbbbbbbaabababbabaababbab" False
          test "aaabbbbbbbbabbbbbbbbbbaabababbabaababbaba" True
  spec_matchSameWithAndWithoutCaching mxToS id matchS "matchS"
  spec_matchSameWithAndWithoutCaching mxToS T.pack tmatchS "tmatchS"

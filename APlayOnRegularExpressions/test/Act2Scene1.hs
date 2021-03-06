module Act2Scene1 (
  spec
)
where

import Semiring
import SpecHelper
import LibAct2

testMatchMX :: (Show a, Eq a)
            => RegMX a -> [a] -> Bool -> SpecWith ()
testMatchMX r as expected =
  it ((if expected then "accep" else "rejec") ++ "ts " ++ show as) $
     matchMX r as `shouldBe` expected

testMatchS :: (Show a, Eq a, Show s, Eq s, Semiring s)
            => Reg s a -> [a] -> s -> SpecWith ()
testMatchS r as expected =
  it ((if (expected == zero) then "rejec" else "accep") ++ "ts " ++ show as) $
     matchS r as `shouldBe` expected

symC :: Semiring s => Char -> Reg s Char
symC c = symS $ \x -> if x == c then one else zero

spec :: Spec
spec = describe "A Play on Regular Expressions, Act 2 Scene 1" $ do
  context "has a function split2 that" $ do
    it "preserves its input" $ property prop_split2PreservesInput
    it "splits in roughly equal parts" $ property prop_split2MakesFairHalves
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
      context "sequ" $
        let s1 = sequ "12345"
            s2 = sequ ['a'..'z'] in do
          flip mapM_ ["", "1234", "12345", "12346", "123456"] $ \x ->
            testMatchMX s1 x $ x == "12345"
          flip mapM_ ["", "12", "abcde", "abcde123456"] $ \x ->
            testMatchMX s2 x $ x == "abcdefghijklmnopqrstuvwxyz"
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
    context "even Cs" $
      let noc = RepMX $ AltMX (sym 'a') $ sym 'b'
          onec = SeqMX noc $ sym 'c'
          twocs = SeqMX onec onec
          evencs = SeqMX (RepMX twocs) noc
          test = testMatchMX evencs in do
        test "" True
        test "c" False
        test "cc" True
        test "ccc" False
        test "cccc" True
        test "acbbba" False
        test "babababcabababbbabbabbabcbabbabbababbbbba" True
        test "acbcbca" False
        test "abcbacbccbaba" True
  context "has a function matchS that matches text for" $ do
    context "basic operators:" $ do
      context "epsS" $ let test = testMatchS epsS in do
        test [] True
        test "a" False
        test "ab" False
      context "symS" $ let testC = testMatchS (symC 'a')
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
      context "altS" $ let test = testMatchS $ altS (symC 'a') (symC 'b') in do
        test "" False
        test "a" True
        test "b" True
        test "c" False
        test "ab" False
        test "aa" False
        test "cc" False
      context "seqS" $ let test = testMatchS $ seqS (symC 'a') (symC 'b') in do
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
    context "more complex regular expressions" $ do
      context "eps*" $ do
        let test = testMatchS $ repS epsS in do
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
          let test = testMatchS aOrEps in do
            test "" True
            test "a" True
            test "b" False
            test "aa" False
        context "(a|eps)*" $
          let test = testMatchS $ repS aOrEps in do
            test "" True
            test "a" True
            test "b" False
            test "aa" True
            test "ba" False
            test "ab" False
        context "a{4}" $
          let test = testMatchS $ a4 in do
            flip mapM_ [0..6] $ \len -> do
              test (replicate len 'a') $ len == 4
              test ('b':(replicate len 'a')) False
              test ((replicate len 'a') ++ "b") False
        context "(a|<eps>)*a{10}" $
          let test = testMatchS $ seqS (repS aOrEps) a10 in do
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
            test = testMatchS $ seqS (seqS (repS a) b) $ seqS aOrB16 $ seqS b $ repS abOrBa in do
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
  spec_matchSameWithAndWithoutCaching $ matchS . mxToS

prop_split2PreservesInput :: [Int] -> Bool
prop_split2PreservesInput list =
  let (lf, rg) = split2 list in (lf ++ rg) == list

prop_split2MakesFairHalves :: [()] -> Bool
prop_split2MakesFairHalves list =
  let (lf, rg) = split2 list in abs (length lf - length rg) <= 1

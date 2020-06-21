module Act2Scene1 (
  spec
)
where

import SpecHelper
import LibAct2

testMatchMX :: (Show a, Eq a)
            => RegMX a -> [a] -> Bool -> SpecWith ()
testMatchMX r as expected =
  it ((if expected then "accep" else "rejec") ++ "ts " ++ show as) $
     matchMX r as `shouldBe` expected

spec :: Spec
spec = describe "A Play on Regular Expressions, Act 2 Scene 1" $ do
  context "has a function split2 that" $ do
    it "preserves its input" $ property prop_split2PreservesInput
    it "splits in roughly equal parts" $ property prop_split2MakesFairHalves
  context "has a function that matches text for" $ do
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
        

prop_split2PreservesInput :: [Int] -> Bool
prop_split2PreservesInput list =
  let (lf, rg) = split2 list in (lf ++ rg) == list

prop_split2MakesFairHalves :: [()] -> Bool
prop_split2MakesFairHalves list =
  let (lf, rg) = split2 list in abs (length lf - length rg) <= 1


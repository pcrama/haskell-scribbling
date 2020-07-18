module Act3Scene1 (
  spec
)
where

import Semiring
import SpecHelper
import LibAct3

testMatchS:: (Show a, Eq a, Show s, Eq s, SemiringEq s)
          => Reg s a -> [a] -> s -> SpecWith ()
testMatchS r as expected =
  it ((if (isSZero expected) then "rejec" else "accep") ++ "ts " ++ show as) $
    matchS r as `shouldBe` expected

spec :: Spec
spec = describe "A Play on Regular Expressions, Act 3 Scene 1" $ do
  context "has a function matchS that" $ do
    let a = symS (=='a')
    let b = symS (=='b')
    let c = symS (=='c')
    context "works on an example: abc" $
      let test = testMatchS (seqS a $ seqS b c) in do
        test "" False
        test "a" False
        test "ab" False
        test "abc" True
        test "aabc" False
        test "abcc" False
    context "works on a context-free grammar: forall n: a{n}b{n}" $
      let anbn = epsS `altS` ((a `seqS` anbn) `seqS` b)
          test = testMatchS anbn in do
        test "" True
        test "c" False
        test "a" False
        test "ab" True
        test "acb" False
        test "acbb" False
        test "aabb" True
        test "abba" False
        test "baabb" False
    spec_matchSameWithAndWithoutCaching $ matchS . mxToS
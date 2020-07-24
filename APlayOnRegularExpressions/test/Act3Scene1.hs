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
    context "works on context-free grammars:" $ do
      context "forall n: a{n}b{n}" $
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
      context "forall n: a{n}b{n}c{n}" $
        let repN 0 _ = epsS
            repN 1 r = r
            repN n r = (repN half r) `seqS` (repN (n - half) r)
              where half = n `div` (2 :: Int)
            bn n = repN n b
            cn n = repN n c
            abncn n = a `seqS` ((bn n `seqS` cn n) `altS` abncn (n + (1 :: Int)))
            test = testMatchS $ epsS `altS` abncn 1 in do
          test "" True
          test "a" False
          test "abc" True
          test "aaaaabbbbbbccccccc" False
          test "aaaaaaaaaabbbbbbbbbbcccccccccc" True
          test "aaaaaaaaaabbbbbbbbbbcccccccccca" False
      context "balanced parens" $
        let left = symS (=='(')
            right = symS (==')')
            notParen = repS $ symS $ \x -> x /= '(' && x /= ')'
            paren = repS $ notParen `seqS` (epsS `altS` ((left `seqS` paren `seqS` right) `seqS` notParen))
            test = testMatchS paren in do
          test "" True
          test "a" True
          test "a(" False
          test "a()" True
          test "a(b)" True
          test "a(b)c" True
          test ")(" False
          test "(((())))" True
          test "((((())))" False
          test "((((a(b)c((d)e)f(((g)h(i)j))k)))lm())" True
    spec_matchSameWithAndWithoutCaching $ matchS . mxToS

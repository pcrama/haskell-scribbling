module Section2 (
  spec
)
where

import SpecHelper
import Lib

spec :: Spec
spec = describe "A Play on Regular Expressions, Section 2" $ do
  context "has a function splits that produces all partitions of a list that" $ do
    it "works for an empty list" $
      splits ([] :: [Int]) `shouldBe` [([], [])]
    it "works for a list with 1 element" $
      splits [1 :: Int] `shouldBe` [([], [1]), ([1], [])]
    it "works for a list with 2 elements" $
      splits "ab" `shouldBe` [([], "ab"), ("a", "b"), ("ab", "")]
    it "works for a list with 3 elements" $
      splits "abc" `shouldBe` [([], "abc"), ("a", "bc"), ("ab", "c"), ("abc", "")]
  it "has my own implementation of accept matching a regular expression" $ do
    accept Eps "" `shouldBe` True
    accept Eps [1 :: Int] `shouldBe` False
    accept (Sym 1) [1 :: Int] `shouldBe` True
    accept (Sym 1) [2 :: Int] `shouldBe` False
    accept (Seq (Seq (Sym 'a') (Sym 'b')) Eps) "ab" `shouldBe` True

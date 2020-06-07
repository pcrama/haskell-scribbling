module Act1Scene1 (
  spec
)
where

import Data.List (nub)

import SpecHelper
import Lib

-- This must be somewhere in standard library but I don't have a connection
-- right now to look for it.
--
-- 2 ** n puts a Float constraint on n.
expt2 :: (Eq t, Num t, Num p) => t -> p
expt2 0 = 1
expt2 n = 2 * expt2 (n - 1)

spec :: Spec
spec = describe "A Play on Regular Expressions, Act 1 Scene 1" $ do
  context "has a function parts that breaks text" $ do
    it "that works for example in pearl" $ do
      parts "acc" `shouldBe` [["acc"], ["a", "cc"], ["ac", "c"], ["a", "c", "c"]]
    it "that splits empty list as a list containing no parts at all" $
      parts "" `shouldBe` [[]]
    it "of length n into parts in 2**(n - 1) different ways" $ do
      flip mapM_ [1, 2, 3, 4, 5] $ \n -> do
        let s = take n [(1 :: Int) ..]
        let p = parts s
        length p `shouldBe` expt2 (n - 1)
        length (nub p) `shouldBe` length p
    it "into parts that can be reassembled" $ do
      flip mapM_ [1, 2, 3, 4, 5] $ \n -> do
        let s = take n [(1 :: Int) ..]
        let p = parts s
        flip mapM_ p $ \x ->
          concat x `shouldBe` s
  context "has a function splits that produces all partitions of a list" $ do
    it "that works for an empty list" $
      splits ([] :: [Int]) `shouldBe` [([], [])]
    it "that works for a list with 1 element" $
      splits [1 :: Int] `shouldBe` [([], [1]), ([1], [])]
    it "that works for a list with 2 elements" $
      splits "ab" `shouldBe` [([], "ab"), ("a", "b"), ("ab", "")]
    it "that works for a list with 3 elements" $
      splits "abc" `shouldBe` [([], "abc"), ("a", "bc"), ("ab", "c"), ("abc", "")]
    it "loses no elements" $
      property prop_splitPreservesInput
  context "has my own implementation of accept matching a regular expression" $ do
    it "that works for Eps" $ do
      accept Eps "" `shouldBe` True
      accept Eps [1 :: Int] `shouldBe` False
    it "that works for Sym" $ do
      accept (Sym 1) [1 :: Int] `shouldBe` True
      accept (Sym 1) [2 :: Int] `shouldBe` False
    it "that works for Alt" $ do
      accept (Alt (Sym 'a') (Sym 'b')) "a" `shouldBe` True
      accept (Alt (Sym 'a') (Sym 'b')) "b" `shouldBe` True
      accept (Alt (Sym 'a') (Sym 'b')) "c" `shouldBe` False
    it "that works for Seq" $ do
      let seqab = (Seq (Sym 'a') (Sym 'b')) in do
        accept seqab "ab" `shouldBe` True
        accept seqab "c" `shouldBe` False
        accept seqab "abc" `shouldBe` False
      let seqabcd = (Seq (Seq (Sym 'a') (Sym 'b'))
                         (Seq Eps (Sym 'd'))) in do
        accept seqabcd "ab" `shouldBe` False
        accept seqabcd "c" `shouldBe` False
        accept seqabcd "abd" `shouldBe` True
        accept seqabcd "abdabd" `shouldBe` False
    it "that works for Rep" $
      let repa = Rep $ Sym 'a' in do
        accept repa "" `shouldBe` True
        accept repa "a" `shouldBe` True
        accept repa "aa" `shouldBe` True
        accept repa "aaaa" `shouldBe` True
        accept repa "aaaaaaaa" `shouldBe` True
        accept repa "b" `shouldBe` False
        accept repa "aab" `shouldBe` False
    it "that works for the examples from the paper" $ do
      accept nocs "aaaabbabababaaaabb" `shouldBe` True
      accept nocs "aaaabbabababaacabb" `shouldBe` False
      accept onec "aaaabbabababaac" `shouldBe` True
      accept onec "aaaabbabababaacc" `shouldBe` False
      accept onec "c" `shouldBe` True
      accept onec "cc" `shouldBe` False
      accept evencs "aaabababa" `shouldBe` True
      accept evencs "ccaaabababa" `shouldBe` True
      accept evencs "aaabababacc" `shouldBe` True
      accept evencs "cccc" `shouldBe` True
      accept evencs "ccac" `shouldBe` False
  context "has a combinator plus" $ do
    it "that works for examples" $
      let re = plus $ Sym 'a' in do
        accept re "" `shouldBe` False
        accept re "b" `shouldBe` False
        accept re "bb" `shouldBe` False
        accept re "a" `shouldBe` True
        accept re "aa" `shouldBe` True
        accept re "aaaa" `shouldBe` True
        accept re "aaaaaaaa" `shouldBe` True
        accept re "aaaaaaaab" `shouldBe` False
  context "has a combinator sequ" $ do
    it "that works for empty sequences" $
      let re = sequ "" in do
        accept re "" `shouldBe` True
        accept re "b" `shouldBe` False
    it "that works for one element sequences" $
      let re = sequ [1 :: Int] in do
        accept re [] `shouldBe` False
        accept re [2] `shouldBe` False
        accept re [1, 2] `shouldBe` False
        accept re [1] `shouldBe` True
        accept re [1, 1] `shouldBe` False
    it "that works for a sequence" $
      let re = sequ "xyzzy" in do
        accept re "xyzzy" `shouldBe` True
        accept re "b" `shouldBe` False
        accept re "xyzzyx" `shouldBe` False

prop_splitPreservesInput :: [Int] -> Bool
prop_splitPreservesInput list =
  and $ map ((== list) . uncurry (++)) $ splits list

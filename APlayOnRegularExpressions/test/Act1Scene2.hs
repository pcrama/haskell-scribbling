module Act1Scene2 (
  spec
)
where

import SpecHelper
import Lib
import Semiring

spec :: Spec
spec = describe "A Play on Regular Expressions, Act 1 Scene 2" $ do
  context "has a function acceptA1S2" $ do
    it "that works for examples with Bool output type" $ do
      let r = weighted $ sequ $ "abc"
      acceptA1S2 r "acbc" `shouldBe` False
      acceptA1S2 r "abc" `shouldBe` True
      acceptA1S2 r "abcd" `shouldBe` False
      acceptA1S2 r "ab" `shouldBe` False
      acceptA1S2 r "" `shouldBe` False
    it "that works for examples with Int output type" $ do
      let r = weighted $ sequ $ "abc"
      acceptA1S2 r "acbc" `shouldBe` (0 :: Int)
      acceptA1S2 r "abc" `shouldBe` (1 :: Int)
      acceptA1S2 r "abcd" `shouldBe` (0 :: Int)
      acceptA1S2 r "ab" `shouldBe` (0 :: Int)
      acceptA1S2 r "" `shouldBe` (0 :: Int)
      let s = AltW (weighted $ Sym (23 :: Int)) (SymW $ const one)
      acceptA1S2 s [] `shouldBe` (0 :: Int)
      acceptA1S2 s [34] `shouldBe` (1 :: Int)
      acceptA1S2 s [23] `shouldBe` (2 :: Int)

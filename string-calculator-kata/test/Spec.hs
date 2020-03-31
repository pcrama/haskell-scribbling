module Main (main) where

import           Test.Hspec

import qualified StringCal as S

main :: IO ()
main = hspec $ do
  it "returns 0 for an empty string" $ S.add "" `shouldBe` 0
  it "returns 1 for \"1\"" $ S.add "1" `shouldBe` 1
  it "returns 3 for \"1,2\"" $ S.add "1,2" `shouldBe` 3
  it "returns 4 for \"4\"" $ S.add "4" `shouldBe` 4
  it "returns 5 for \"2,3\"" $ S.add "2,3" `shouldBe` 5
  it "ignores whitespace" $ S.add " 2 , 3 " `shouldBe` 5

module Main
where

import SpecHelper
import qualified Act1Scene1
import qualified Act1Scene2
import qualified SemiringSpec

main :: IO ()
main = hspec $ do
  Act1Scene1.spec
  Act1Scene2.spec
  SemiringSpec.spec

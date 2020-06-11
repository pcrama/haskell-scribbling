module Main
where

import SpecHelper
import qualified Act1Scene1
import qualified SemiringSpec

main :: IO ()
main = hspec $ do
  Act1Scene1.spec
  SemiringSpec.spec

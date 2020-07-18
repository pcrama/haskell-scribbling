module Main
where

import SpecHelper
import qualified Act1Scene1
import qualified Act1Scene2
import qualified SemiringSpec
import qualified Act2Scene1
import qualified Act2Scene2
import qualified Act3Scene1

main :: IO ()
main = hspec $ do
  Act1Scene1.spec
  Act1Scene2.spec
  SemiringSpec.spec
  Act2Scene1.spec
  Act2Scene2.spec
  Act3Scene1.spec

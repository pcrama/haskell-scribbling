-- stack exec --package QuickCheck -- ghci test/Spec.hs import Test.Hspec
import Test.HUnit
import Test.QuickCheck
import Test.Hspec
import Test.Hspec.Core.QuickCheck (modifyMaxSize, modifyMaxSuccess)

import Data.List.NonEmpty (NonEmpty(..))
import Text.ParserCombinators.ReadP

import ParseRipSpec

testParseCRS s exp =
  assertEqual ("testPqrseCRS of " ++ s) [(exp, "")] $ readP_to_S (parseCRS 0) s

main :: IO ()
main = hspec $ do
  describe "parseCRS" $ do
    it "should work for examples" $ do
      testParseCRS "- album: a" $ CRS { info = (Album, "a") :| []
                                      , overrides = [] }

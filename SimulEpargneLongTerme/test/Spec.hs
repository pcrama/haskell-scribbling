import Banking
  ( Amount(..)
  , Transaction
  , compound
  , fiaBalance
  , fiaDeposit
  , fiaNew
  )
import Data.Monoid ((<>))
import Data.Time.Calendar
  ( Day
  , addDays
  , diffDays
  , fromGregorian
  , toGregorian
  )

import Debug.Trace (trace)

-- stack exec --package QuickCheck -- ghci test/Spec.hs
import Test.Hspec
import Test.HUnit
-- import Test.QuickCheck
-- import Test.Hspec.Core.QuickCheck (modifyMaxSize, modifyMaxSuccess)

main :: IO ()
main = hspec $ do
  describe "compound" $
    it "should compound nearly the same in two steps or only one" $ do
      let a = Amount 1000
          r = 0.01
          d1 = fromGregorian 2000 1 1
          d2 = addDays 3000 d1
          d3 = addDays 4000 d2
        in compound a r d1 d3 @=? compound (compound a r d1 d2) r d2 d3

  describe "FixedInterestAccount" $
    it "should add up the money" $ do
      fiaBalance (fiaNew 0.02) (fromGregorian 2012 10 10) @=? Amount 0
  describe "FixedInterestAccount" $
    it "should add up the money" $ do
      let x = Amount 1234
          y = Amount 4321
          d = fromGregorian 2012 10 10
          t1 = Transaction x d "t1"
          t2 = Transaction y d "t2"
          t12 = Transaction (x <> y) d "t1+t2"
        in do
          fiaBalance (fiaDeposit (fiaDeposit (fiaNew 0.02) t1) t2) d ?=@ (x <> y)
          fiaBalance (fiaDeposit (fiaDeposit (fiaNew 0.02) t1) t2) d @=? fiaBalance (fiaDeposit (fiaNew 0.02) t12) d

-- Local Variables:
-- intero-targets: "SimulEpargneLongTerme:test:SimulEpargneLongTerme-test"
-- End:

module OwnMatchGroup (
  spec
)
where

import Data.Function (on)
import Data.List (nubBy, sortBy)
import Control.Monad (replicateM)

import LibOwn
import Semiring (Semiring(..))
import SemiringSpec (
  Proxy(..)
  , semiringLaws
  )
import SpecHelperOwn

newtype ArbMatchGroup = AMG MatchGroup
  deriving (Show, Eq)

instance Arbitrary ArbMatchGroup where
  arbitrary = do
      NonNegative count <- arbitrary
      AMG <$> oneof [return NoMG, return $ MG $ MI [], (MG . MI . sortNub) <$> replicateM ((count `mod` 20) + 1) genMI]
    where genMI = do
            NonNegative ai <- arbitrary
            NonNegative ab <- arbitrary
            x <- arbitrary
            return (ai, ab, if (x < 0) then Nothing else (Just $ ab + x))
          sortNub = nubBy ((==) `on` fst3) . sortBy (compare `on` fst3)
          fst3 (x, _, _) = x
  shrink (AMG NoMG) = []
  shrink (AMG (MG (MI []))) = [AMG NoMG]
  shrink (AMG (MG (MI as))) = map (AMG . MG . MI) $ []:(foldr clearEnd [] as):shrunk as
    where shrunk [] = []
          shrunk [_] = []
          shrunk (x:y:zs) = (x:zs):(y:zs):shrunk zs
          clearEnd (ai, ab, Just _) tl = (ai, ab, Nothing):tl
          clearEnd _                tl = tl

instance Semiring ArbMatchGroup where
  zero = AMG zero
  one = AMG one
  (AMG x) `splus` (AMG y) = AMG $ x `splus` y
  (AMG x) `stimes` (AMG y) = AMG $ x `stimes` y

spec :: Spec
spec = describe "Own extension (MatchGroup)" $ do
  context "defines a type MatchGroup that" $
    semiringLaws (Proxy :: Proxy ArbMatchGroup)
  context "debug distributivity 1" $ do
    let x = AMG (MG (MI [(1 :: Int, 0 :: Int, Just (1 :: Int))]))
    let y = AMG (MG (MI []))
    let z = AMG (MG (MI [(1,1,Just 1)]))
    let r = AMG (MG (MI [(1 :: Int, 0 :: Int, Just (1 :: Int)), (1 :: Int, 1 :: Int, Just (1 :: Int))]))
    it "(x+y)*z" $ ((x `splus` y) `stimes` z) `shouldBe` r
    it "(x*z)+(y*z)" $ ((x `stimes` z) `splus` (y `stimes` z)) `shouldBe` r
  context "debug distributivity 2" $ do
    let x = AMG (MG (MI []))
    let y = AMG (MG (MI [(14 :: Int, 10 :: Int, Just (14 :: Int))]))
    let z = AMG (MG (MI [(14,18,Just 27)]))
    let r = AMG (MG (MI [(14 :: Int, 10 :: Int, Just (14 :: Int)), (14 :: Int, 18 :: Int, Just (27 :: Int))]))
    it "(x+y)*z" $ ((x `splus` y) `stimes` z) `shouldBe` r
    it "x*z" $ (x `stimes` z) `shouldBe` (AMG $ MG $ MI [(14, 18, Just 27)])
    it "y*z" $ (y `stimes` z) `shouldBe` (AMG $ MG $ MI [(14, 10, Just 14), (14, 18, Just 27)])
    it "(x*z)+(y*z)" $ ((x `stimes` z) `splus` (y `stimes` z)) `shouldBe` r
  context "defines mergeMatchInfo" $ do
    it "example mergeMatchInfo [(12,10,Just 16)] [(12,5,Nothing)]" $
      mergeMatchInfo [(12,10,Just 16)] [(12,5,Nothing)] `shouldBe` [(12 :: Int, 10 :: Int, Just (16 :: Int))]
    it "example mergeMatchInfo [(12,5,Nothing)] [(12,10,Just 16)]" $
      mergeMatchInfo [(12,5,Nothing)] [(12,10,Just 16)] `shouldBe` [(12 :: Int, 10 :: Int, Just (16 :: Int))]
    it "example mergeMatchInfo [(12,10,Just 16)] [(12,5,Just 7)]" $
      mergeMatchInfo [(12,10,Just 16)] [(12,5,Just 7)] `shouldBe` [(12, 5, Just 7), (12 :: Int, 10 :: Int, Just (16 :: Int))]
    it "example mergeMatchInfo [(12,5,Just 7)] [(12,10,Just 16)]" $
      mergeMatchInfo [(12,5,Just 7)] [(12,10,Just 16)] `shouldBe` [(12, 5, Just 7), (12 :: Int, 10 :: Int, Just (16 :: Int))]
    it "example mergeMatchInfo [(14,2,Just 15)] $ mergeMatchInfo [] [(14,15,Just 15)]" $
      (mergeMatchInfo [(14,2,Just 15)] $ mergeMatchInfo [] [(14,15,Just 15)]
        ) `shouldBe` mergeMatchInfo [(14,2,Just 15)] [(14 :: Int, 15 :: Int, Just (15 :: Int))]
    it "example mergeMatchInfo [(14,2,Just 15)] [(14,15,Just 15)]" $
      mergeMatchInfo [(14,2,Just 15)] [(14,15,Just 15)] `shouldBe` [(14, 2, Just 15), (14 :: Int, 15 :: Int, Just (15 :: Int))]
    it "example mergeMatchInfo [(4,2,Just 5)] $ mergeMatchInfo [] [(4,12,Just 15)]" $
      (mergeMatchInfo [(4,2,Just 5)] $ mergeMatchInfo [] [(4,12,Just 15)]
        ) `shouldBe` mergeMatchInfo [(4,2,Just 5)] [(4 :: Int, 12 :: Int, Just (15 :: Int))]
    it "example mergeMatchInfo [(4,2,Just 5)] [(4,12,Just 15)]" $
      mergeMatchInfo [(4,2,Just 5)] [(4,12,Just 15)] `shouldBe` [(4, 2, Just 5), (4 :: Int, 12 :: Int, Just (15 :: Int))]
    it "example mergeMatchInfo [(24,2,Just 15)] $ mergeMatchInfo [] [(24,15,Just 115)]" $
      (mergeMatchInfo [(24,2,Just 15)] $ mergeMatchInfo [] [(24,15,Just 115)]
        ) `shouldBe` mergeMatchInfo [(24,2,Just 15)] [(24 :: Int, 15 :: Int, Just (115 :: Int))]
    it "example mergeMatchInfo [(24,2,Just 15)] [(24,15,Just 115)]" $
      mergeMatchInfo [(24,2,Just 15)] [(24,15,Just 115)] `shouldBe` [(24 :: Int, 2 :: Int, Just (15 :: Int)), (24 :: Int, 15 :: Int, Just (115 :: Int))]

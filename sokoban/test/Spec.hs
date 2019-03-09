module Main where

import Data.Maybe (isJust, maybe)
import Test.Hspec

import Zipper

import TestInteractiveSelect
import TestGame

testZipper :: SpecWith ()
testZipper = describe "Zipper" $ do
    describe ("mkZipper " ++ show initList) $ do
      let mbZ0 = mkZipper initList
      it "makes a Zipper for a non empty list" $ shouldBe (isJust mbZ0) True
      let Just z0 = mbZ0
      it "focusses on first element" $ shouldBe (zipperFocus z0) (initList !! 0)
      let mbZ1 = zipperNext z0
      it "can move forward" $ shouldBe (maybe 99 zipperFocus mbZ1) (initList !! 1)
      let Just z1 = mbZ1
      let mbZ2 = zipperNext z1
      it "can move forward again" $ shouldBe (maybe 99 zipperFocus mbZ2) (initList !! 2)
      let Just z2 = mbZ2
      it "doesn't move past end" $ shouldBe (zipperNext z2) Nothing
      let mbZ1' = zipperPrev z2
      it "can move back" $ shouldBe (maybe 99 zipperFocus mbZ1') (initList !! 1)
      let Just z1' = mbZ1'
      let mbZ0' = zipperPrev z1'
      it "can move back again" $ shouldBe (maybe 99 zipperFocus mbZ0') (initList !! 0)
      let Just z0' = mbZ0'
      it "doesn't move before start" $ shouldBe (zipperPrev z0') Nothing
    describe "mkZipper \"\"" $ do
      it "doesn't make invalid Zippers" $ shouldBe (mkZipper "") Nothing
  where initList = [0..2 :: Int]

main :: IO ()
main = hspec $ do
  testZipper
  testInteractiveSelect
  describe "Game" $ do
    testUnconstrainedMove
    testMakeMap
    testMove
    testPlayLevel
    testPlayGame
    testWon

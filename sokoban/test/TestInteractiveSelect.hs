module TestInteractiveSelect (testInteractiveSelect) where

{- TODO:
 - Add property checking:
 - 1. TestCalls start with Draw, then alternates between Draw & Query
 - 2. Last Draw x always is focus of Zipper
 - 3. If last TestCall is ConfirmSelection, Just output and Zipper content are the same
 - 4. If last TestCall is QuitSelection, output is Nothing
 -}

import Control.Monad.RWS (runRWS, RWS, get, put, tell)
import Test.Hspec

import Zipper
import InteractiveSelect

data TestState a = TestState [SelectCommand] (Zipper a)
  deriving (Show, Eq)

data TestCalls a = Query SelectCommand | Draw a
  deriving (Show, Eq)

type TestM a o = RWS () [TestCalls a] (TestState a) o

query' :: TestM a SelectCommand
query' = do
  TestState (c:cmds) z <- get
  put $ TestState cmds z
  tell $ [Query c]
  return c

draw' :: a -> TestM a ()
draw' = tell . (:[]) . Draw

selector :: Bool -> TestM a (Maybe a)
selector = interactiveSelect (\(TestState _ z) -> z) (\z (TestState c _) -> TestState c z) draw' query'

testInteractiveSelect :: SpecWith ()
testInteractiveSelect = describe "interactiveSelect" $ do
    describe "scenario 1" $
      runScenario [0..2 :: Int] False [Draw 0, Query ConfirmSelection] 0 (Just 0)
    describe "scenario 2" $
      runScenario [0..2 :: Int] True [Draw 1, Query ConfirmSelection] 1 (Just 1)
    describe "scenario 3" $
      runScenario [0..2 :: Int] 
                  False 
                  [Draw 0, Query PrevElt, Draw 0, Query ConfirmSelection] 
                  0
                  (Just 0)
    describe "scenario 4" $
      runScenario [0..2 :: Int]
                  True
                  [Draw 1, Query PrevElt, Draw 0, Query ConfirmSelection] 
                  0
                  (Just 0)
    describe "scenario 5" $
      runScenario [0..3 :: Int]
                  False
                  [Draw 0, Query PrevElt, Draw 0, Query NextElt, Draw 1
                  , Query LastElt, Draw 3, Query NextElt, Draw 3
                  , Query PrevElt, Draw 2, Query FirstElt, Draw 0
                  , Query LastElt, Draw 3, Query ConfirmSelection]
                  3
                  (Just 3)
    describe "1 choice only" $
      runScenario "a"
                  True
                  [Draw 'a', Query PrevElt, Draw 'a', Query NextElt, Draw 'a'
                  , Query LastElt, Draw 'a', Query NextElt, Draw 'a'
                  , Query PrevElt, Draw 'a', Query FirstElt, Draw 'a'
                  , Query LastElt, Draw 'a', Query ConfirmSelection]
                  'a'
                  (Just 'a')
  where isQuery (Query _) = True
        isQuery (Draw _) = False
        runScenario choices initSkip expLogging expFocus expResult = do
          let cmds = map (\(Query q) -> q) $ filter isQuery expLogging
          let Just zIn = mkZipper choices
          let (output, TestState execCmds zOut, logging) = runRWS (selector initSkip) () $ TestState cmds zIn
          it "ran all steps" $ shouldBe execCmds []
          it "called all expected actions" $ shouldBe logging expLogging
          it "focussed on selected element" $ shouldBe (zipperFocus zOut) expFocus
          it "return the correct value" $ shouldBe output expResult

module TestGame (
    testMakeMap
  , testMove
  , testPlayLevel
  , testUnconstrainedMove
  , testPlayGame
  , testWon
  -- for CheckDistributionOfArb.hs
  , ArbMap(..)
  , ArbPlayerCommand
  , TestLevelCalls(..)
  , extractPlayerCommands
  , runPlayLevelScenario
  , tileChanges
  )
where

import Data.List (find)
import Data.Maybe (isNothing, isJust, catMaybes)
import Control.Monad (forM_)
import Control.Monad.RWS (runRWS, RWS, get, put, tell)
import Test.Hspec
import Test.QuickCheck

import Game

-- shorthand to avoid passing the maximum number of undos all the time
-- (not really important for our tests anyway)
makeMap' :: [String] -> Maybe Map
makeMap' = makeMap 50

newtype ArbMap = ArbMap Map

instance Show ArbMap where
  show (ArbMap mp) = show mp ++ concat ['\n':[encode $ tile mp $ Pos { _x = c, _y = r }
                                             | c <- [0.._cols mp - 1]]
                                       | r <- [0.._rows mp - 1]]
    where encode (F Free) = '.'
          encode (F Target) = '_'
          encode (O CrateOnFree) = 'X'
          encode (O CrateOnTarget) = 'x'
          encode Wall = '#'

instance Arbitrary ArbMap where
  arbitrary = do
      (NonNegative r', NonNegative c') <- arbitrary -- (arbitrary :: Gen (NonNegative Int, NonNegative Int))
      let rows = 3 + (r' `mod` 20)
      let cols = 3 + (c' `mod` 20)
      (NonNegative px', NonNegative py') <- arbitrary -- (arbitrary :: Gen (NonNegative Int, NonNegative Int))
      let px = px' `mod` cols
      let py = py' `mod` rows
      NonNegative undosLeft <- arbitrary -- (arbitrary :: Gen (NonNegative Int))
      -- traverse :: (a -> f b) -> t a -> f (t b)
      tiles <- traverse (makeRow px py cols) [0..rows - 1]
      return $ ArbMap $ Map {
          _rows = rows
        , _cols = cols
        , _player = Pos { _x = px, _y = py }
        , _moveMap = \Pos { _x = x, _y = y } ->
                       if x < 0 || x >= cols || y < 0 || y >= rows
                       then Wall
                       else (tiles !! y) !! x
        , _undosLeft = undosLeft `mod` 10
        , _undo = Nothing }
    where makeRow :: Int -> Int -> Int -> Int -> Gen [Tile]
          makeRow px py cols r = traverse (makeCol px py r) [0..cols - 1]
          makeCol :: Int -> Int -> Int -> Int -> Gen Tile
          makeCol px py r c
            | c == px && r == py = return $ F Free -- place for player
            | otherwise = frequency [(16, return $ F Free)
                                    , (3, return $ F Target)
                                    , (2, return $ O CrateOnFree)
                                    , (2, return $ O CrateOnTarget)
                                    , (2, return Wall)]

newtype ArbPlayerCommand = ArbPlayerCommand { unArbPlayerCommand :: PlayerCommand }
  deriving Show

instance Arbitrary ArbPlayerCommand where
  arbitrary = fmap ArbPlayerCommand $ frequency [(20, return $ Move Up)
                                                , (20, return $ Move Do)
                                                , (20, return $ Move Ri)
                                                , (20, return $ Move Le)
                                                , (3, return Undo)
                                                , (1, return Quit)]

-- ensure arbitrary list of commands is never empty and always finishes with
-- Quit to make sure all arbitrary levels finish
extractPlayerCommands :: [ArbPlayerCommand] -> [PlayerCommand]
extractPlayerCommands apc = map unArbPlayerCommand apc ++ [Quit]

runPlayLevelForProperty :: ArbMap -> [ArbPlayerCommand] -> [TestLevelCalls]
runPlayLevelForProperty (ArbMap mp) apc =
  let _cmds = extractPlayerCommands apc
      (_levelResult, _remainingCmds, logging) = runRWS (playLevel mp query' draw') () _cmds
  in logging

containsNoTestLevelFailures :: [TestLevelCalls] -> Bool
containsNoTestLevelFailures = isNothing . find isTestLevelFailure
  where isTestLevelFailure (TestLevelFailure _) = True
        isTestLevelFailure (Draw _ _) = False
        isTestLevelFailure (Query _) = False

playLevelProperty :: String -> (Map -> [TestLevelCalls] -> Bool) -> SpecWith ()
playLevelProperty descr prop = it descr $ property p
  where p amp@(ArbMap mp) apc =
          let logging = runPlayLevelForProperty amp apc
          in prop mp logging && containsNoTestLevelFailures logging

playerMoves :: Map -> [TestLevelCalls] -> Int
playerMoves initialMap = snd . foldr update (initPos, 0)
  where initPos = _player initialMap
        update (Query _) x = x
        update (TestLevelFailure _) x = x
        update (Draw _ newPos) x@(prevPos, count)
          | newPos == prevPos = x
          | otherwise         = (newPos, count + 1)

tileChanges :: Map -> [TestLevelCalls] -> Int
tileChanges initialMap = snd . foldr update (initTiles, 0)
  where (initTiles, _) = extractMapInfo initialMap
        update (Query _) x = x
        update (TestLevelFailure _) x = x
        update (Draw newTiles _) x@(prevTiles, count)
          | newTiles == prevTiles = x
          | otherwise             = (newTiles, count + 1)

-- TODO: study inputs to see if we cover enough
prop_tilesChangeLessThanPlayerMoves :: Map -> [TestLevelCalls] -> Bool
prop_tilesChangeLessThanPlayerMoves mp logging =
  tileChanges mp logging <= playerMoves mp logging

-- TODO: study inputs to see if we cover enough
prop_playLevelConservesCounts :: Map -> [TestLevelCalls] -> Bool
prop_playLevelConservesCounts mp logging =
    (all countsMatch $ catMaybes $ map getDraw $ logging)
  where getDraw (Draw ts _) = Just ts
        getDraw (Query _) = Nothing
        getDraw (TestLevelFailure _) = Nothing
        isFree (F _) = 1 :: Int
        isFree _ = 0
        isTarget (F Target) = 1 :: Int
        isTarget (O CrateOnTarget) = 1 :: Int
        isTarget _ = 0
        isWall Wall = 1 :: Int
        isWall _ = 0
        isCrate (O _) = 1 :: Int
        isCrate _ = 0
        -- coords = [Pos { _x = c, _y = r } | c <- [0..cols - 1], r <- [0..rows - 1]]
        countTiles p m = sum $ map (sum . map p) m
        countFree = countTiles isFree
        countWall = countTiles isWall
        countTarget = countTiles isTarget
        countCrate = countTiles isCrate
        mpTiles = fst $ extractMapInfo mp
        initFree = countFree mpTiles
        initWall = countWall mpTiles
        initTarget = countTarget mpTiles
        initCrate = countCrate mpTiles
        countsMatch m = countFree m == initFree
                     && countWall m == initWall
                     && countTarget m == initTarget
                     && countCrate m == initCrate

-- TODO: study inputs to see if we cover enough
prop_playLevelPlayerOnlyStepsOnFreeTiles :: Map -> [TestLevelCalls] -> Bool
prop_playLevelPlayerOnlyStepsOnFreeTiles _ logging =
    (all playerOnFreeTiles $ catMaybes $ map getDraw $ logging)
  where getDraw (Draw ts p) = Just (ts, p)
        getDraw (Query _) = Nothing
        getDraw (TestLevelFailure _) = Nothing
        playerOnFreeTiles (ts, Pos { _x = x, _y = y }) =
            y >= 0 && x >= 0 && y < length ts && x < length row
            && isFree (row !! x)
          where row = ts !! y
                isFree (F _) = True
                isFree (O _) = False
                isFree Wall = False

-- TODO: study inputs to see if we cover enough
prop_playLevelDrawAndQueryAlternate :: Map -> [TestLevelCalls] -> Bool
prop_playLevelDrawAndQueryAlternate _ logging = alternatingDrawAndQuery logging
  where alternatingDrawAndQuery [] = True
        alternatingDrawAndQuery [_] = True
        -- must always start with drawing then querying player for his next move
        alternatingDrawAndQuery ((Draw _ _):(Query _):ls) = alternatingDrawAndQuery ls
        alternatingDrawAndQuery _ = False

testMakeMap, testMove, testPlayLevel, testUnconstrainedMove, testPlayGame :: SpecWith ()

testUnconstrainedMove = describe "unconstrainedMove" $ do
  let p0 = Pos { _x = 0, _y = 0 }
  let p1 = unconstrainedMove p0 Up
  let p2 = unconstrainedMove p1 Le
  let p3 = unconstrainedMove p2 Do
  let p4 = unconstrainedMove p3 Ri
  let p5 = unconstrainedMove p4 Up
  it "circles round" $ do
    p0 `shouldBe` p4
    p1 `shouldBe` p5
  it "moves" $ do
    p0 `shouldNotBe` p1
    p1 `shouldNotBe` p2
    p2 `shouldNotBe` p3
    p3 `shouldNotBe` p4
    p0 `shouldNotBe` p2
    p0 `shouldNotBe` p3
  it "moves back and forth" $ do
    p0 `shouldBe` (unconstrainedMove (unconstrainedMove p0 Up) Do)
    p0 `shouldBe` (unconstrainedMove (unconstrainedMove p0 Do) Up)
    p0 `shouldBe` (unconstrainedMove (unconstrainedMove p0 Le) Ri)
    p0 `shouldBe` (unconstrainedMove (unconstrainedMove p0 Ri) Le)

minimalMapText :: [String]
minimalMapText = ["#####", "#_X*#", "#####"]
minimalMap :: [[Tile]]
minimalMap = [[Wall, Wall, Wall, Wall, Wall]
             , [Wall, F Target, O CrateOnFree, F Free, Wall]
             , [Wall, Wall, Wall, Wall, Wall]]
minimalMapPos :: Pos
minimalMapPos = Pos { _x = 3, _y = 1 }

testMakeMap = describe "makeMap" $ do
  describe "ignores invalid input" $ do
    it "not rectangular" $
      makeMap' ["#####", "#*X_#", "###"] `shouldSatisfy` isNothing
    it "with 2 initial player positions" $
      makeMap' ["#####", "#*X_*", "#####"] `shouldSatisfy` isNothing
    it "without initial player position" $
      makeMap' ["#####", "# X_#", "#####"] `shouldSatisfy` isNothing
  it "parses minimal Map" $ do
    let Just mp = makeMap' minimalMapText
    _rows mp `shouldBe` length minimalMapText
    _cols mp `shouldBe` length (head minimalMapText)
    _player mp `shouldBe` minimalMapPos
    forM_ [0..2] $ \row ->
      forM_ [0..4] $ \col ->
        tile mp (Pos { _x = col, _y = row }) `shouldBe` (minimalMap !! row) !! col
  it "recognizes all tile types" $ do
    let Just mp = makeMap' ["#Xx_* "]
    _rows mp `shouldBe` 1
    _cols mp `shouldBe` 6
    _player mp `shouldBe` Pos { _x = 4, _y = 0 }
    forM_ [(0, Wall), (1, O CrateOnFree), (2, O CrateOnTarget)
          , (3, F Target), (4, F Free), (5, F Free)]
        $ \(col, expTile) -> do
          tile mp (Pos { _x = col, _y = 0 }) `shouldBe` expTile
  it "treats garbage as free tiles" $ do
    let Just mp = makeMap' $ ".###.":(tail minimalMapText)
    _rows mp `shouldBe` length minimalMapText
    _cols mp `shouldBe` length (head minimalMapText)
    _player mp `shouldBe` minimalMapPos
    forM_ [(0, F Free), (1, Wall), (2, Wall), (3, Wall), (4, F Free)] $ \(col, expTile) ->
      tile mp (Pos { _x = col, _y = 0 }) `shouldBe` expTile
    forM_ [1..2] $ \row ->
      forM_ [0..4] $ \col ->
        tile mp (Pos { _x = col, _y = row }) `shouldBe` (minimalMap !! row) !! col

type TestLevelState = [PlayerCommand]

data TestLevelCalls = Query PlayerCommand
                    | Draw [[Tile]] Pos
                    | TestLevelFailure String
  deriving (Show, Eq)

type TestLevelM o = RWS () [TestLevelCalls] TestLevelState o

query' :: TestLevelM PlayerCommand
query' = do
  cmds' <- get
  case cmds' of
    c:cmds -> do
      put $ cmds
      tell [Query c]
      return c
    [] -> do
      -- should never happen:
      -- 1. in scripted scenarios (testPlayLevel scenarios) it is the
      --    responsibility of the scenario author,
      -- 2. for properties, all lists of arbitrary commands go through
      --    extractPlayerCommands and have a trailing Quit.
      tell [TestLevelFailure "Unexpected end of commands in query"]
      return Quit -- try to end test case

draw' :: Map -> TestLevelM ()
draw' = tell . (:[]) . uncurry Draw . extractMapInfo

runPlayLevelScenario :: Map                 -- |^ level's map
                     -> [TestLevelCalls]    -- |^ get simulated user interactions from expected logging
                     -> (Bool
                        , TestLevelState
                        , [TestLevelCalls]) -- |^ (playLevel return value, final state, logging)
runPlayLevelScenario mp expLogging = do
    let cmds = catMaybes $ map keepOnlyQuery expLogging
    runRWS (playLevel mp query' draw') () cmds
  where keepOnlyQuery (Query x) = Just x
        keepOnlyQuery (Draw _ _) = Nothing
        keepOnlyQuery (TestLevelFailure _) = Nothing

testPlayLevel = describe "playLevel" $ do
    describe "properties" $ do
      playLevelProperty "conserves tile counts" prop_playLevelConservesCounts
      playLevelProperty "only steps on free tiles" prop_playLevelPlayerOnlyStepsOnFreeTiles
      playLevelProperty "draws map then waits for user input in loop" prop_playLevelDrawAndQueryAlternate
      playLevelProperty "changes tiles at most as often as player moves" prop_tilesChangeLessThanPlayerMoves
    runScenario "scenario 1"
                minimalMapText
                [Draw minimalMap minimalMapPos
                , Query $ Move Le
                , Draw [[Wall, Wall, Wall, Wall, Wall]
                       , [Wall, O CrateOnTarget, F Free, F Free, Wall]
                       , [Wall, Wall, Wall, Wall, Wall]]
                     $ Pos { _x = 2, _y = 1 }]
                True
    runScenario "scenario 2"
                minimalMapText
                [Draw minimalMap minimalMapPos
                , Query $ Quit]
                False
    runScenario "scenario 3"
                ["_X " ,"*X " ,"  _"]
                [Draw [[F Target, O CrateOnFree, F Free]
                      ,[F Free, O CrateOnFree, F Free]
                      ,[F Free, F Free, F Target]]
                    $ Pos { _x = 0, _y = 1 }
                , Query $ Move Ri
                , Draw [[F Target, O CrateOnFree, F Free]
                       ,[F Free, F Free, O CrateOnFree]
                       ,[F Free, F Free, F Target]]
                     $ Pos { _x = 1, _y = 1 }
                , Query $ Move Ri
                , Draw [[F Target, O CrateOnFree, F Free]
                       ,[F Free, F Free, O CrateOnFree]
                       ,[F Free, F Free, F Target]]
                     $ Pos { _x = 1, _y = 1 }
                , Query $ Move Up
                , Draw [[F Target, O CrateOnFree, F Free]
                       ,[F Free, F Free, O CrateOnFree]
                       ,[F Free, F Free, F Target]]
                     $ Pos { _x = 1, _y = 1 }
                , Query $ Move Le
                , Draw [[F Target, O CrateOnFree, F Free]
                       ,[F Free, F Free, O CrateOnFree]
                       ,[F Free, F Free, F Target]]
                     $ Pos { _x = 0, _y = 1 }
                , Query $ Move Up
                , Draw [[F Target, O CrateOnFree, F Free]
                       ,[F Free, F Free, O CrateOnFree]
                       ,[F Free, F Free, F Target]]
                     $ Pos { _x = 0, _y = 0 }
                , Query $ Move Ri
                , Draw [[F Target, F Free, O CrateOnFree]
                       ,[F Free, F Free, O CrateOnFree]
                       ,[F Free, F Free, F Target]]
                     $ Pos { _x = 1, _y = 0 }
                , Query $ Undo
                , Draw [[F Target, O CrateOnFree, F Free]
                       ,[F Free, F Free, O CrateOnFree]
                       ,[F Free, F Free, F Target]]
                     $ Pos { _x = 0, _y = 0 }
                , Query $ Undo
                , Draw [[F Target, O CrateOnFree, F Free]
                       ,[F Free, O CrateOnFree, F Free]
                       ,[F Free, F Free, F Target]]
                     $ Pos { _x = 0, _y = 1 }
                , Query $ Move Do
                , Draw [[F Target, O CrateOnFree, F Free]
                       ,[F Free, O CrateOnFree, F Free]
                       ,[F Free, F Free, F Target]]
                     $ Pos { _x = 0, _y = 2 }
                , Query Quit]
                False
  where runScenario title mapText expLogging expResult = describe title $ do
          let (Just mp) = makeMap' mapText
          let (result, execCmds, logging) = runPlayLevelScenario mp expLogging
          it "ran all steps" $ execCmds `shouldBe` []
          it "called all expected actions" $ logging `shouldBe` expLogging
          it "return the correct value" $ result `shouldBe` expResult

testMove = describe "move & moveCrate" $ do
    -- several tests will use a 5x5 map with the player in the center,
    -- surrounded by crates and pushing them in the 4 directions.  The
    -- `pushCrateAwayFromCenter' lookup table described the expected
    -- position of the player and the expected position of the crate.
    let pushCrateAwayFromCenter =
               [(Up, (Pos { _x = 2, _y = 1 }, Pos { _x = 2, _y = 0 }))
               , (Do, (Pos { _x = 2, _y = 3 }, Pos { _x = 2, _y = 4 }))
               , (Le, (Pos { _x = 1, _y = 2 }, Pos { _x = 0, _y = 2 }))
               , (Ri, (Pos { _x = 3, _y = 2 }, Pos { _x = 4, _y = 2 }))]
    forM_ [Up, Do, Le, Ri] $ \dir ->
      describe ("move <Map...> " ++ show dir) $ do
        it "doesn't run through walls" $ do
          let Just mp = makeMap' ["###", "#*#", "###"]
          move mp dir `shouldSatisfy` isNothing
        it "doesn't push crates through walls" $ do
          let Just mp = makeMap' ["#####", "#XXX#", "#X*X#", "#XXX#", "#####"]
          move mp dir `shouldSatisfy` isNothing
        it "doesn't push more than one crate" $ do
          let Just mp = makeMap' ["XXXXX", "XXXXX", "XX*XX", "XXXXX", "XXXXX"]
          move mp dir `shouldSatisfy` isNothing
        it "pushes 1! crate onto F Free" $ do
          let Just mp = makeMap' [".....", ".XXX.", ".X*X.", ".XXX.", "....."]
          let Just (shouldBeFree, shouldBeCrate) = lookup dir pushCrateAwayFromCenter
          let mbMp' = move mp dir
          mbMp' `shouldSatisfy` isJust
          let Just mp' = mbMp'
          tile mp' shouldBeFree `shouldBe` F Free
          tile mp' shouldBeCrate `shouldBe` O CrateOnFree
          _player mp' `shouldBe` shouldBeFree
        it "pushes 1! crate onto F Target" $ do
          let Just mp = makeMap' [".._..", ".XXX.", "_X*X_", ".XXX.", ".._.."]
          let Just (shouldBeFree, shouldBeCrate) = lookup dir pushCrateAwayFromCenter
          let mbMp' = move mp dir
          mbMp' `shouldSatisfy` isJust
          let Just mp' = mbMp'
          tile mp' shouldBeFree `shouldBe` F Free
          tile mp' shouldBeCrate `shouldBe` O CrateOnTarget
          _player mp' `shouldBe` shouldBeFree
        it "pushes 1! crate from F Target onto F Free" $ do
          let Just mp = makeMap' [".....", ".XxX.", ".x*x.", ".XxX.", "....."]
          let Just (shouldBeTarget, shouldBeCrate) = lookup dir pushCrateAwayFromCenter
          let mbMp' = move mp dir
          mbMp' `shouldSatisfy` isJust
          let Just mp' = mbMp'
          tile mp' shouldBeTarget `shouldBe` F Target
          tile mp' shouldBeCrate `shouldBe` O CrateOnFree
          _player mp' `shouldBe` shouldBeTarget

data TestGameQuery = SelectLevel Bool [String]
                   | PlayLevel PlayerCommand
                   | Prompt Bool
  deriving (Show, Eq)

data TestGameLog = GQuery TestGameQuery
                 | GDraw [[Tile]] Pos
                 | TestGameFailure String
  deriving (Show, Eq)

type TestGameM o = RWS () [TestGameLog] [TestGameQuery] o

-- Can simulate aborting the level selection process by passing an
-- invalid map to makeMap
testGameSelectLevel :: Bool -> TestGameM (Maybe Map)
testGameSelectLevel b = do
  cmds' <- get
  case cmds' of
    (SelectLevel _ levelText):cmds -> do
      put $ cmds
      tell [GQuery $ SelectLevel b levelText]
      return $ makeMap' levelText
    [] -> do
      tell [TestGameFailure "Reached end of commands in testGameSelectLevel"]
      return Nothing -- try to stop test run
    c:cmds -> do -- if this happens, the test driver and system under test have gotten out of sync
      put $ cmds
      tell [TestGameFailure $ "Unexpected " ++ show c ++ " in testGameSelectLevel"]
      return Nothing -- try to stop test run

testGameQuery :: TestGameM PlayerCommand
testGameQuery = do
  cmds' <- get
  case cmds' of
    taggedC@(PlayLevel c):cmds -> do
      put $ cmds
      tell [GQuery taggedC]
      return c
    [] -> do
      tell [TestGameFailure "Reached end of commands in testGameQuery"]
      return Quit -- try to stop test run
    c:cmds -> do -- if this happens, the test driver and system under test have gotten out of sync
      put $ cmds
      tell [TestGameFailure $ "Unexpected " ++ show c ++ " in testGameQuery"]
      return Quit -- try to stop test run

testGamePrompt :: String -> TestGameM Bool
testGamePrompt _ = do
  cmds' <- get
  case cmds' of
    taggedB@(Prompt b):cmds -> do
      put $ cmds
      tell [GQuery taggedB]
      return b
    [] -> do
      tell [TestGameFailure "Reached end of commands in testGamePrompt"]
      return False -- try to stop test run
    c:cmds -> do -- if this happens, the test driver and system under test have gotten out of sync
      put $ cmds
      tell [TestGameFailure $ "Unexpected " ++ show c ++ " in testGamePrompt"]
      return False -- try to stop test run

testGameDraw :: Map -> TestGameM ()
testGameDraw = tell . (:[]) . uncurry GDraw . extractMapInfo

extractMapInfo :: Map -> ([[Tile]], Pos)
extractMapInfo mp@(Map { _rows = rows, _cols = cols, _player = p }) =
    ([[tile mp $ Pos { _x = c, _y = r } | c <- [0..cols - 1]]
     | r <- [0..rows - 1]]
    , p)

testPlayGame = describe "playGame" $ do
    describe "scenario 1: win 1 level, abort next level" $ do
      runScenario [GQuery $ SelectLevel False minimalMapText
                  , GDraw minimalMap minimalMapPos
                  , GQuery $ PlayLevel $ Move Ri -- tries to go through Wall
                  , GDraw minimalMap minimalMapPos -- no effect on level state
                  , GQuery $ PlayLevel $ Move Le
                  , GDraw [[Wall, Wall, Wall, Wall, Wall]
                          , [Wall,O CrateOnTarget, F Free, F Free, Wall]
                          , [Wall, Wall, Wall, Wall, Wall]]
                        $ Pos { _x = 2, _y = 1 }
                  , GQuery $ Prompt True
                  , selectImpossibleLevel True
                  , drawImpossibleLevel
                  , GQuery $ PlayLevel $ Quit
                  , GQuery $ Prompt False]
    describe "scenario 2: abort level selection" $ do
      runScenario [GQuery $ SelectLevel False []]
    describe "scenario 3: start level, abort it, retry same level, then quit" $ do
      runScenario [selectImpossibleLevel False
                  , drawImpossibleLevel
                  , GQuery $ PlayLevel $ Quit
                  , GQuery $ Prompt True
                  , selectImpossibleLevel False
                  , drawImpossibleLevel
                  , GQuery $ PlayLevel $ Quit
                  , GQuery $ Prompt False]
    describe "scenario 4: start level, abort it back to level selection, then quit" $ do
      runScenario [selectImpossibleLevel False
                  , drawImpossibleLevel
                  , GQuery $ PlayLevel $ Quit
                  , GQuery $ Prompt True
                  , GQuery $ SelectLevel False []]
  where selectImpossibleLevel b = GQuery $ SelectLevel b ["X*_"]
        drawImpossibleLevel = GDraw [[O CrateOnFree, F Free, F Target]] $ Pos { _x = 1, _y = 0 }
        isQuery (GQuery _) = True
        isQuery (GDraw _ _) = False
        isQuery (TestGameFailure _) = False
        playGame' = playGame testGameSelectLevel testGameQuery testGameDraw testGamePrompt
        runScenario expLogging = do
          let cmds = map (\(GQuery x) -> x) $ filter isQuery expLogging
          let (_, execCmds, logging) = runRWS playGame' () cmds
          it "ran all steps" $ execCmds `shouldBe` []
          -- this will also implicitly reject any TestGameFailure
          -- in logging
          it "called all expected actions" $ logging `shouldBe` expLogging

testWon :: SpecWith ()
testWon = describe "won" $ do
    it "works when #crates > #targets" $ do
      ["X*x"] `shouldSatisfy` wins -- because there is nowhere to put the left crate
      ["X*", "X_"] `shouldNotSatisfy` wins
    it "works when #crates = #targets" $ do
      ["X*x_"] `shouldNotSatisfy` wins -- because there is still room for the left crate
      [".x.", "x*x", ".x."] `shouldSatisfy` wins
    it "works when #crates < #targets" $ do
      ["_*x_"] `shouldSatisfy` wins -- because there are no other crates to fill the other targets
      ["_*X_"] `shouldNotSatisfy` wins
  where wins s = case makeMap' s of
                   Just mp -> won mp
                   Nothing -> error $ "Bad test case: map " ++ show s ++ " could not be parsed"

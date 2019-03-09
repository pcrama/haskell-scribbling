module TestGame (
    testMakeMap
  , testMove
  , testPlayLevel
  , testUnconstrainedMove
  , testPlayGame
  , testWon
  )
where

import Data.Maybe (isNothing, isJust, catMaybes)
import Control.Monad (forM_)
import Control.Monad.RWS (runRWS, RWS, get, put, tell)
import Test.Hspec
import Test.QuickCheck

import Game

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
            | otherwise = frequency [(8, return $ F Free) 
                                    , (1, return $ F Target)
                                    , (1, return $ O CrateOnFree)
                                    , (1, return $ O CrateOnTarget)
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

-- TODO: study inputs to see if we cover enough
prop_playLevelConservesCounts :: ArbMap -> [ArbPlayerCommand] -> Bool
prop_playLevelConservesCounts (ArbMap mp) apc =
    all countsMatch $ catMaybes $ map getDraw $ logging
  where cmds = map unArbPlayerCommand apc ++ [Quit] -- ensure list of commands is never empty
        (_, _, logging) = runRWS (playLevel mp query' draw') () cmds
        getDraw (Draw ts _) = Just ts
        getDraw (Query _) = Nothing
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
prop_playLevelPlayerOnlyStepsOnFreeTiles :: ArbMap -> [ArbPlayerCommand] -> Bool
prop_playLevelPlayerOnlyStepsOnFreeTiles (ArbMap mp) apc =
    all playerOnFreeTiles $ catMaybes $ map getDraw $ logging
  where cmds = map unArbPlayerCommand apc ++ [Quit] -- ensure list of commands is never empty
        (_, _, logging) = runRWS (playLevel mp query' draw') () cmds
        getDraw (Draw ts p) = Just (ts, p)
        getDraw (Query _) = Nothing
        playerOnFreeTiles (ts, Pos { _x = x, _y = y }) =
            y >= 0 && x >= 0 && y < length ts && x < length row
            && isFree (row !! x)
          where row = ts !! y
                isFree (F _) = True
                isFree (O _) = False
                isFree Wall = False

-- TODO: study inputs to see if we cover enough
prop_playLevelDrawAndQueryAlternate :: ArbMap -> [ArbPlayerCommand] -> Bool
prop_playLevelDrawAndQueryAlternate (ArbMap mp) apc =
    alternatingDrawAndQuery logging
  where cmds = map unArbPlayerCommand apc ++ [Quit] -- ensure list of commands is never empty
        (_, _, logging) = runRWS (playLevel mp query' draw') () cmds
        alternatingDrawAndQuery [] = True
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
      makeMap ["#####", "#*X_#", "###"] `shouldSatisfy` isNothing
    it "with 2 initial player positions" $
      makeMap ["#####", "#*X_*", "#####"] `shouldSatisfy` isNothing
    it "without initial player position" $
      makeMap ["#####", "# X_#", "#####"] `shouldSatisfy` isNothing
  it "parses minimal Map" $ do
    let Just mp = makeMap minimalMapText
    _rows mp `shouldBe` length minimalMapText
    _cols mp `shouldBe` length (head minimalMapText)
    _player mp `shouldBe` minimalMapPos
    forM_ [0..2] $ \row ->
      forM_ [0..4] $ \col ->
        tile mp (Pos { _x = col, _y = row }) `shouldBe` (minimalMap !! row) !! col
  it "recognizes all tile types" $ do
    let Just mp = makeMap ["#Xx_* "]
    _rows mp `shouldBe` 1
    _cols mp `shouldBe` 6
    _player mp `shouldBe` Pos { _x = 4, _y = 0 }
    forM_ [(0, Wall), (1, O CrateOnFree), (2, O CrateOnTarget)
          , (3, F Target), (4, F Free), (5, F Free)]
        $ \(col, expTile) -> do
          tile mp (Pos { _x = col, _y = 0 }) `shouldBe` expTile
  it "treats garbage as free tiles" $ do
    let Just mp = makeMap $ ".###.":(tail minimalMapText)
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
  deriving (Show, Eq)

type TestLevelM o = RWS () [TestLevelCalls] TestLevelState o

query' :: TestLevelM PlayerCommand
query' = do
  c:cmds <- get
  put $ cmds
  tell $ [Query c]
  return c

draw' :: Map -> TestLevelM ()
draw' = tell . (:[]) . uncurry Draw . extractMapInfo

testPlayLevel = describe "playLevel" $ do
    describe "properties" $ do
      it "conserves tile counts" $ property prop_playLevelConservesCounts
      it "only steps on free tiles" $ property prop_playLevelPlayerOnlyStepsOnFreeTiles
      it "draws map then waits for user input in loop" $ property prop_playLevelDrawAndQueryAlternate
    describe "scenario 1" $
      runScenario minimalMapText
                  [Draw minimalMap minimalMapPos
                  , Query $ Move Le
                  , Draw [[Wall, Wall, Wall, Wall, Wall]
                         , [Wall, O CrateOnTarget, F Free, F Free, Wall]
                         , [Wall, Wall, Wall, Wall, Wall]]
                         $ Pos { _x = 2, _y = 1 }]
                  True
    describe "scenario 2" $
      runScenario minimalMapText
                  [Draw minimalMap minimalMapPos
                  , Query $ Quit]
                  False
    describe "scenario 3" $
      runScenario ["_X " ,"*X " ,"  _"]
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
  where isQuery (Query _) = True
        isQuery (Draw _ _) = False
        runScenario mapText expLogging expResult = do
          let cmds = map (\(Query x) -> x) $ filter isQuery expLogging
          let Just mp = makeMap mapText
          let (result, execCmds, logging) = runRWS (playLevel mp query' draw') () cmds
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
          let Just mp = makeMap ["###", "#*#", "###"]
          move mp dir `shouldSatisfy` isNothing
        it "doesn't push crates through walls" $ do
          let Just mp = makeMap ["#####", "#XXX#", "#X*X#", "#XXX#", "#####"]
          move mp dir `shouldSatisfy` isNothing
        it "doesn't push more than one crate" $ do
          let Just mp = makeMap ["XXXXX", "XXXXX", "XX*XX", "XXXXX", "XXXXX"]
          move mp dir `shouldSatisfy` isNothing
        it "pushes 1! crate onto F Free" $ do
          let Just mp = makeMap [".....", ".XXX.", ".X*X.", ".XXX.", "....."]
          let Just (shouldBeFree, shouldBeCrate) = lookup dir pushCrateAwayFromCenter
          let mbMp' = move mp dir
          mbMp' `shouldSatisfy` isJust
          let Just mp' = mbMp'
          tile mp' shouldBeFree `shouldBe` F Free
          tile mp' shouldBeCrate `shouldBe` O CrateOnFree
          _player mp' `shouldBe` shouldBeFree
        it "pushes 1! crate onto F Target" $ do
          let Just mp = makeMap [".._..", ".XXX.", "_X*X_", ".XXX.", ".._.."]
          let Just (shouldBeFree, shouldBeCrate) = lookup dir pushCrateAwayFromCenter
          let mbMp' = move mp dir
          mbMp' `shouldSatisfy` isJust
          let Just mp' = mbMp'
          tile mp' shouldBeFree `shouldBe` F Free
          tile mp' shouldBeCrate `shouldBe` O CrateOnTarget
          _player mp' `shouldBe` shouldBeFree
        it "pushes 1! crate from F Target onto F Free" $ do
          let Just mp = makeMap [".....", ".XxX.", ".x*x.", ".XxX.", "....."]
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
  deriving (Show, Eq)

type TestGameM o = RWS () [TestGameLog] [TestGameQuery] o

-- Can simulate aborting the level selection process by passing an
-- invalid map to makeMap
testGameSelectLevel :: Bool -> TestGameM (Maybe Map)
testGameSelectLevel b = do
  (SelectLevel _ levelText):cmds <- get
  put $ cmds
  tell $ [GQuery $ SelectLevel b levelText]
  return $ makeMap levelText

testGameQuery :: TestGameM PlayerCommand
testGameQuery = do
  taggedC@(PlayLevel c):cmds <- get
  put $ cmds
  tell $ [GQuery taggedC]
  return c

testGamePrompt :: String -> TestGameM Bool
testGamePrompt _ = do
  taggedC@(Prompt b):cmds <- get
  put $ cmds
  tell $ [GQuery taggedC]
  return b

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
        playGame' = playGame testGameSelectLevel testGameQuery testGameDraw testGamePrompt
        runScenario expLogging = do
          let cmds = map (\(GQuery x) -> x) $ filter isQuery expLogging
          let (_, execCmds, logging) = runRWS playGame' () cmds
          it "ran all steps" $ execCmds `shouldBe` []
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
  where wins s = case makeMap s of
                   Just mp -> won mp
                   Nothing -> error $ "Bad test case: map " ++ show s ++ " could not be parsed"

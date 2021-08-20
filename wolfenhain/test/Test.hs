{-# LANGUAGE TypeApplications #-}
module Main (
  main
)

where

import Data.List (nub)
import Control.Monad (guard)
import Debug.Trace (trace)
import Test.QuickCheck
import Test.Hspec

import LineOfFlight
import Tagged
import Terrain


traceShow :: Show a => String -> a -> p -> p
-- traceShow label x v = trace (label <> show x) v
traceShow _ _ v = v


traceTerrain :: ArbitraryTerrain -> ObstacleHit -> ObstacleHit -> p -> p
traceTerrain t o1 o2 = trace (renderTerrain t o1 o2)
-- traceTerrain _ _ _ v = v


traceFailureWithTerrain :: ArbitraryTerrain -> ObstacleHit -> ObstacleHit -> Bool -> Bool
traceFailureWithTerrain a o1 o2 b
  | b = True
  | otherwise = traceTerrain a o1 o2 False


data Row
data Col


data ArbitraryTerrain = ArbitraryTerrain { atRows :: Tagged Row Int
                                         , atCols :: Tagged Col Int
                                         , atAssocs :: [(Tagged Row Int, Tagged Col Int)]
                                         , atX :: Tagged Col Double
                                         , atY :: Tagged Row Double
                                         , atAngle :: Double }
  deriving (Show, Eq)


buildTerrainArray :: ArbitraryTerrain -> R1st2D
buildTerrainArray ArbitraryTerrain { atRows = Tagged rows
                                   , atCols = Tagged cols
                                   , atAssocs = assocs }
  = rowFirst2DArray rows
                    cols
                  $ map toWall assocs --  [(r, c, 1) | (r :: Tagged Row Int, c :: Tagged Row Int) <- assocs]
  where toWall :: (Tagged Row Int, Tagged Col Int) -> (Int, Int, Int)
        toWall (Tagged r, Tagged c) = (r, c, 1)

evaluateTerrain :: ArbitraryTerrain -> ObstacleHit
evaluateTerrain terrain@(ArbitraryTerrain { atX = Tagged x
                                          , atY = Tagged y
                                          , atAngle = angle })
  = lineOfFlight x y angle $ buildTerrainArray terrain


renderTerrain :: ArbitraryTerrain -> ObstacleHit -> ObstacleHit -> String
renderTerrain a@(ArbitraryTerrain { atRows = Tagged rows
                                  , atCols = Tagged cols
                                  , atX = Tagged x
                                  , atY = Tagged y })
              o1
              o2
  | (rows + cols) > 20 = joinWithSep lineSep ["(R: " <> show rows <> ", C: " <> show cols <> ")", show o1, show o2]
  | otherwise = joinWithSep lineSep $ show a:show o1:show o2:"":colNumbers:numberedRows
  where width = 3
        lineSep = "\n"
        colMax = cols - 1
        rowMax = rows - 1
        prependRowNumber h t = h <> " " <> t
        colNumbers :: String
        colNumbers = prependRowNumber (replicate width ' ') $ foldMap (center . show) [0..colMax]
        numberedRows :: [String]
        numberedRows = map renderRow [rowMax, rowMax-1..0]
        renderRow rowNumber = prependRowNumber (rightAlign rowNumber)
                                             $ foldMap (center . (:"") . renderCell rowNumber) [0..colMax]
        renderCell r c = case (r12d terrain r c, round x == c && round y == r) of
                           (0, True) -> '@'
                           (0, False) -> ' '
                           (_, True) -> '!'
                           (_, False) -> '#'
        joinWithSep sep = drop (length sep) . foldMap (sep <>)
        rightAlign b = let sb = show b in replicate (width - length sb) ' ' ++ sb
        center sa = let w2 = (width - length sa) `div` 2
                        wr = width - length sa - w2
                    in replicate w2 ' ' <> sa <> replicate wr ' '
        terrain = buildTerrainArray a


equalObstacleHit :: ObstacleHit -> ObstacleHit -> Bool
equalObstacleHit Horizon Horizon = True
equalObstacleHit Horizon (Obstacle _ _ _ _) = False
equalObstacleHit (Obstacle _ _ _ _) Horizon = False
equalObstacleHit (Obstacle d a b f) (Obstacle p m n r) = traceShow "equalObstacleHit: " (show d <> " " <> show p)
                                                       $ diffDistance < 0.1
                                                      && 100 * diffDistance < meanDistance
                                                      && a == m && b == n
                                                      && (abs $ f - r) < 0.01
  where diffDistance = abs $ d - p
        meanDistance = (d + p) / 2.0


equalDistance :: ObstacleHit -> ObstacleHit -> Bool
equalDistance Horizon Horizon = True
equalDistance Horizon (Obstacle _ _ _ _) = False
equalDistance (Obstacle _ _ _ _) Horizon = False
equalDistance (Obstacle x _ _ _) (Obstacle y _ _ _) = traceShow "equalDistance: " (show x <> " " <> show y) $ diffDistance < 0.1 && 100 * diffDistance < meanDistance
  where diffDistance = abs $ x - y
        meanDistance = (x + y) / 2.0


isHorizon :: ObstacleHit -> Bool
isHorizon Horizon = True
isHorizon (Obstacle _ _ _ _) = False


isDistance :: (Double -> Bool) -> ObstacleHit -> Bool
isDistance _ Horizon = False
isDistance p (Obstacle d _ _ _) = p d


prop_distanceConstantUnderSomeTransformation :: TerrainTransformation -> ArbitraryTerrain -> Property
prop_distanceConstantUnderSomeTransformation xform x =
    checkCoverage $
    cover 5 (isHorizon d1) "horizon" $
    cover 10 (isDistance (> 5) d1) "distance > 5" $
    cover 5 (isSide 0 d1) "east side" $
    cover 5 (isSide 1 d1) "north side" $
    cover 5 (isSide 2 d1) "west side" $
    cover 5 (isSide 3 d1) "south side" $
    d1 `equalDistance` d2 && d1 `equalObstacle` d2
  where (TerrainTransformationD fI _ _) = xform x
        d1 = evaluateTerrain x
        d2 = evaluateTerrain $ transformTerrain xform x
        isSide _ Horizon = False
        isSide n (Obstacle _ _ _ t) = (n <= t && t <= n + 1) || (t == n + 4) || (t == n - 4)
        equalObstacle Horizon Horizon = True
        equalObstacle (Obstacle _ xh yh _) (Obstacle _ x' y' _)
            = fI (Tagged @Row yh) (Tagged @Col xh) == (Tagged @Row y', Tagged @Col x')
        equalObstacle (Obstacle _ _ _ _) Horizon = False
        equalObstacle Horizon (Obstacle _ _ _ _) = False


fmod :: Double -> Int -> Double
fmod x y = x - (fromIntegral $ (floor x `div` y) * y)


fmodTagged :: Tagged t Double -> Tagged t Int -> Tagged t Double
fmodTagged (Tagged x) (Tagged y) = Tagged $ x `fmod` y


roundTagged :: Tagged t Double -> Tagged t Int
roundTagged = Tagged . round . getTagged


instance Arbitrary ArbitraryTerrain where
  arbitrary = do
    arbRows <- arbitrary
    arbCols <- arbitrary
    arbX <- arbitrary
    arbY <- arbitrary
    arbAngle <- arbitrary
    let rows = arbRows + 3 -- at least 3 rows
    let cols = arbCols + 3 -- at least 3 columns
    let x = arbX `fmodTagged` cols
    let y = arbY `fmodTagged` rows
    let intX = roundTagged x
    let intY = roundTagged y
    let angle = arbAngle `fmod` 13
    assocs <- oneof [return $ [p
                              | c <- [0..cols - 1]
                              , p <- [(Tagged $ getTagged c `mod` 2, c)
                                     , (rows - (Tagged $ getTagged c `mod` 2) - 1, c)]]
                           ++ [p
                              | r <- [0..rows - 1]
                              , p <- [(r, Tagged $ getTagged r `mod` 2)
                                     , (r, cols - (Tagged $ getTagged r `mod` 2) - 1)]]
                    , do
                        NonNegative arbThreshold <- arbitrary
                        randomOffsets <- arbitrary
                        let threshold = (arbThreshold `mod` 10 + 2)
                        return [(r, c)
                               | ((r, c), o) <- zip [(r, c) | r <- [0..rows - 1], c <- [0..cols - 1]]
                                                  $ map (\(NonNegative ro) -> (ro `mod` 15) - (2 :: Int))
                                                      $ cycle $ NonNegative 0:randomOffsets
                               , (getTagged r + getTagged c) `mod` 16 >= threshold + o]]
    return ArbitraryTerrain { atRows = rows
                            , atCols = cols
                            , atAssocs = [(r `rem` rows, c `rem` cols)
                                         | (r, c) <- assocs
                                         , r /= intY || c /= intX]
                            , atX = x
                            , atY = y
                            , atAngle = angle}
  shrink (ArbitraryTerrain { atAssocs = [] }) = []
  shrink at@(ArbitraryTerrain { atRows = rows, atCols = cols, atAssocs = assocs, atAngle = angle, atX = x, atY = y }) =
      shrinkRows ++ shrinkCols ++ shrinkWalls ++ shrinkAngle ++ shrinkRounding (x, y) (coordRounder 10) coordSetter ++ shrinkRounding angle (doubleRounder 10) angleSetter
    where shrinkRows
            | rows `mod` 2 == 0 = shrunkTop at ++ shrunkBottom at
            | otherwise = shrunkBottom at ++ shrunkTop at
          shrinkCols
            | cols `mod` 2 == 0 = shrunkLeft at ++ shrunkRight at
            | otherwise = shrunkRight at ++ shrunkLeft at
          shrinkWalls = case assocs of
                          [] -> []
                          [_] -> [at { atAssocs = [] }]
                          [v, w] ->  [at { atAssocs = [v] }, at { atAssocs = [w] }]
                          (z:zs) -> do
                                      shrunkAssocs <- ([z]:shrinkOddEvenGo assocs [] []) ++ [zs]
                                      guard $ null shrunkAssocs
                                      return $ at { atAssocs = shrunkAssocs }
          shrinkOddEvenGo [] xs ys = [xs, ys]
          shrinkOddEvenGo (z:zs) xs ys = shrinkOddEvenGo zs ys $ z:xs
          twoPi = 2 * pi
          shrinkAngle
            | angle < 0 = [transformTerrain horizontalAxisSymmetry at]
            | angle >= twoPi = [at { atAngle = angle - twoPi }]
            | angle >= pi = let bt = transformTerrain verticalAxisSymmetry at
                                ct@(ArbitraryTerrain { atAngle = cAngle }) = transformTerrain horizontalAxisSymmetry bt
                            in [ct { atAngle = cAngle - twoPi }]
            | angle >= pi/2 = let bt = transformTerrain transpose at
                              in [transformTerrain horizontalAxisSymmetry bt]
            | otherwise = []
          shrinkRounding :: Eq a => a -> (a -> a) -> (a -> ArbitraryTerrain) -> [ArbitraryTerrain]
          shrinkRounding val rounder setter =
            let rounded = rounder val
            in if rounded == val then [] else [setter rounded]
          doubleRounder :: Double -> Double -> Double
          doubleRounder factor xf = fromIntegral (round $ xf * factor :: Int) / factor
          coordRounder :: Double -> (Tagged Col Double, Tagged Row Double) ->  (Tagged Col Double, Tagged Row Double)
          coordRounder factor (Tagged xf, Tagged yf) = (Tagged @Col $ doubleRounder factor xf
                                                       , Tagged @Row $ doubleRounder factor yf)
          angleSetter a' = at { atAngle = a' }
          coordSetter (x', y') = at { atX = x', atY = y' }


shrunkTop, shrunkBottom, shrunkLeft, shrunkRight :: ArbitraryTerrain -> [ArbitraryTerrain]
shrunkTop (ArbitraryTerrain { atRows = rows
                            , atCols = cols
                            , atAssocs = assocs
                            , atX = x
                            , atY = y
                            , atAngle = angle })
  | roundY >= newRows || (rows <= 3 && roundY > 0) || (rows <= 2 && roundY == 0) = []
  | otherwise = case [(r, c) | (r, c) <- assocs, r < newRows] of
                  [] -> []
                  shrunkAssocs -> [ArbitraryTerrain { atRows = newRows
                                                    , atCols = cols
                                                    , atAssocs = shrunkAssocs
                                                    , atX = x
                                                    , atY = y
                                                    , atAngle = angle }]
  where roundY :: Tagged Row Int
        roundY = roundTagged y
        newRows :: Tagged Row Int
        newRows = rows - 1


shrunkBottom (ArbitraryTerrain { atRows = rows
                               , atCols = cols
                               , atAssocs = assocs
                               , atX = x
                               , atY = y
                               , atAngle = angle })
  | roundY <= 0 || rows <= 3 = []
  | otherwise = let newRows = rows - 1 in
                case [(r - 1, c) | (r, c) <- assocs, r > 0] of
                  [] -> []
                  shrunkAssocs -> [ArbitraryTerrain { atRows = newRows
                                                    , atCols = cols
                                                    , atAssocs = shrunkAssocs
                                                    , atX = x
                                                    , atY = y - 1
                                                    , atAngle = angle }]
  where roundY :: Tagged Row Int
        roundY = roundTagged y


shrunkRight (ArbitraryTerrain { atRows = rows
                              , atCols = cols
                              , atAssocs = assocs
                              , atX = x
                              , atY = y
                              , atAngle = angle })
  | roundX >= newCols || (cols <= 3 && roundX > 0) || (cols <= 2 && roundX <= 0) = []
  | otherwise = case [(r, c) | (r, c) <- assocs, c < newCols] of
                  [] -> []
                  shrunkAssocs -> [ArbitraryTerrain { atRows = rows
                                                    , atCols = newCols
                                                    , atAssocs = shrunkAssocs
                                                    , atX = x
                                                    , atY = y
                                                    , atAngle = angle }]
  where roundX :: Tagged Col Int
        roundX = roundTagged x
        newCols :: Tagged Col Int
        newCols = cols - 1


shrunkLeft (ArbitraryTerrain { atRows = rows
                             , atCols = cols
                             , atAssocs = assocs
                             , atX = x
                             , atY = y
                             , atAngle = angle })
  | roundX <= 0 || cols <= 3 = []
  | otherwise = case [(r, c - 1) | (r, c) <- assocs, c > 0] of
                  [] -> []
                  shrunkAssocs -> [ArbitraryTerrain { atRows = rows
                                                    , atCols = newCols
                                                    , atAssocs = shrunkAssocs
                                                    , atX = x - 1
                                                    , atY = y
                                                    , atAngle = angle }]
  where roundX :: Tagged Col Int
        roundX = roundTagged x
        newCols :: Tagged Col Int
        newCols = cols - 1


transformSize :: (Tagged Row Int -> Tagged Col Int -> (Tagged Row Int, Tagged Col Int)) -- ^ transform row col to (row', col')
              -> Tagged Row Int -- ^ rows
              -> Tagged Col Int -- ^ cols
              -> (Tagged Row Int, Tagged Col Int) -- ^ size (rows', cols') of ([0..rows), [0..cols)) after transformation
transformSize f rows cols = (1 + max r1 r2, 1 + max c1 c2)
  where (r1, c1) = f 0 0
        (r2, c2) = f (rows - 1) (cols - 1)


data TerrainTransformationD = TerrainTransformationD (Tagged Row Int -> Tagged Col Int -> (Tagged Row Int, Tagged Col Int))
                                                     (Tagged Col Double -> Tagged Row Double -> (Tagged Col Double, Tagged Row Double))
                                                     (Double -> Double)


type TerrainTransformation = ArbitraryTerrain -> TerrainTransformationD


transpose, verticalAxisSymmetry, horizontalAxisSymmetry :: TerrainTransformation
transpose _ = TerrainTransformationD swapTagged swapTagged (\angle -> pi / 2.0 - angle)
  where swapTagged :: Tagged o a -> Tagged p a -> (Tagged o a, Tagged p a)
        swapTagged (Tagged o) (Tagged p) = (Tagged p, Tagged o)

-- | Symmetry around a vertical axis
verticalAxisSymmetry (ArbitraryTerrain { atCols = cols })
    = TerrainTransformationD (\r c -> (r, cols - 1 - c))
                             (\x y -> (fromIntegral cols - 1 - x, y))
                             (\angle -> pi - angle)


-- | Symmetry around a horizontal axis
horizontalAxisSymmetry (ArbitraryTerrain { atRows = rows })
    = TerrainTransformationD (\r c -> (rows - 1 - r, c))
                             (\x y -> (x, fromIntegral rows - 1 - y))
                             (\angle -> 0 - angle)


transformTerrain :: TerrainTransformation -> ArbitraryTerrain -> ArbitraryTerrain
transformTerrain xform
                 at@(ArbitraryTerrain { atCols = cols
                                      , atRows = rows
                                      , atAssocs = assocs
                                      , atX = x
                                      , atY = y
                                      , atAngle = angle })
  = at { atCols = cols'
       , atRows = rows'
       , atAssocs = map (uncurry fI) assocs
       , atX = x'
       , atY = y'
       , atAngle = fA angle
       }
    where (TerrainTransformationD fI fD fA) = xform at
          (rows', cols') = transformSize fI rows cols
          (x', y') = fD x y


-- | Compute distance to next obstacle analytically, assuming the terrain is simply a closed rectangle
analyticalSolution :: Tagged Row Int -- ^ Rows
                   -> Tagged Col Int -- ^ Cols
                   -> Tagged Col Double -- ^ x
                   -> Tagged Row Double -- ^ y
                   -> Double -- ^ angle
                   -> Double
analyticalSolution (Tagged rows) (Tagged cols) (Tagged x) (Tagged y) angle =
    traceShow "analyticalSolution: " (rows, cols, x, y, angle) $
    case traceShow "analyticalSolution (dX, dY): " (distanceX, distanceY) (distanceX, distanceY) of
      (Just a, Just b) -> min a b
      (Just a, Nothing) -> a
      (Nothing, Just b) -> b
      (Nothing, Nothing) -> error "Unit vector with 0 length!"
  where u = cos angle
        v = sin angle
        limit rangeMax vectorComponent
          | vectorComponent >= 0 = fromIntegral rangeMax - 1.5
          | otherwise = 0.5
        xlimit = limit cols u
        ylimit = limit rows v
        distance component vectorComponent componentLimit
          | vectorComponent == 0 = Nothing
          | otherwise = Just $ abs $ (component - componentLimit) / vectorComponent
        distanceX = distance x u xlimit
        distanceY = distance y v ylimit


prop_analyticalDistanceCalculation :: ArbitraryTerrain -> Bool
prop_analyticalDistanceCalculation at@(ArbitraryTerrain { atRows = rows
                                                        , atCols = cols
                                                        , atX = x
                                                        , atY = y
                                                        , atAngle = angle }) =
    equalDistance oh $ Obstacle analyticalDistance undefined undefined undefined
  where oh = evaluateTerrain $ at { atAssocs = [(r, 0) | r <- [0..rows - 1]]
                                            ++ [(r, cols - 1) | r <- [0..rows - 1]]
                                            ++ [(0, c) | c <- [0..cols - 1]]
                                            ++ [(rows - 1, c) | c <- [0..cols - 1]]
                                  , atX = xMappedIntoTerrain
                                  , atY = yMappedIntoTerrain }
        analyticalDistance = analyticalSolution rows cols xMappedIntoTerrain yMappedIntoTerrain angle
        xMappedIntoTerrain = 0.501 + (x `fmodTagged` (fromIntegral $ cols - 2)) * 0.9
        yMappedIntoTerrain = 0.501 + (y `fmodTagged` (fromIntegral $ rows - 2)) * 0.9


distanceSmallerOrEqual :: ObstacleHit -> ObstacleHit -> Bool
distanceSmallerOrEqual Horizon Horizon = True
distanceSmallerOrEqual Horizon (Obstacle _ _ _ _) = False
distanceSmallerOrEqual (Obstacle _ _ _ _) Horizon = True
distanceSmallerOrEqual (Obstacle d1 _ _ _) (Obstacle d2 _ _ _) = d1 <= d2


prop_distanceGrowsWhenRemovingObstacles :: ArbitraryTerrain -> Property
prop_distanceGrowsWhenRemovingObstacles at@(ArbitraryTerrain { atAssocs = assocs }) =
    checkCoverage $
    cover 1 (isHorizon d1) "horizon (meaningless test)" $
    cover 50 (isDistance (const True) d1) "distance" $
    prop d1
  where d1 = evaluateTerrain at
        prop Horizon = True
        prop (Obstacle _ x y _) = distanceSmallerOrEqual d1
                                                       $ evaluateTerrain
                                                       $ at { atAssocs = filter (not . obstacleNeighbour x y) assocs }
        obstacleNeighbour x y (r, c) = (abs $ r - Tagged @Row y) <= 1 && (abs $ c - Tagged @Col x) <= 1


addObstacles :: [(Tagged Row Int, Tagged Col Int)] -> ArbitraryTerrain -> ObstacleHit -> Maybe ArbitraryTerrain
addObstacles _ _ Horizon = Nothing
addObstacles rowColOffs
             at@(ArbitraryTerrain { atRows = rows
                                  , atCols = cols
                                  , atX = x
                                  , atY = y
                                  , atAssocs = assocs })
             (Obstacle _ c r _) = Just newTerrain
  where newTerrain = at { atAssocs = nub $ assocs
                                        ++ [(r', c')
                                           | (ro, co) <- rowColOffs
                                           , let r' = Tagged @Row r + ro
                                           , r' >= 0 && r' < rows
                                           , let c' = Tagged @Col c + co
                                           , c' >= 0 && c' < cols
                                           , not $ roundY == r' && roundX == c'] }
        roundX = roundTagged x
        roundY = roundTagged y


add8Obstacles, add4Obstacles :: ArbitraryTerrain -> ObstacleHit -> Maybe ArbitraryTerrain
add8Obstacles = addObstacles [(-1, 0), (1, 0), (0, -1), (0, 1)
                             ,(-1,-1), (1, 1), (1, -1), (-1,1)]
add4Obstacles = addObstacles [(-1, 0), (1, 0), (0, -1), (0, 1)]


distanceLargerOrEqual :: ObstacleHit -> ObstacleHit -> Double -> Bool
distanceLargerOrEqual o p maxDiff = distanceSmallerOrEqual p o
                                 && case (o, p) of
                                      (Obstacle dO _ _ _, Obstacle dP _ _ _) ->
                                        dP <= dO && dO - dP <= maxDiff
                                      (Horizon, Horizon) -> True
                                      _ -> False


makePropertyDistanceDecreasesWhenAddingObstacles :: (ArbitraryTerrain -> ObstacleHit -> Maybe ArbitraryTerrain)
                                                 -> Double
                                                 -> (ArbitraryTerrain -> Property)
makePropertyDistanceDecreasesWhenAddingObstacles addObst factor = p
  where fuzzedFactor = 1.00001 * factor
        p at@(ArbitraryTerrain { atAngle = angle }) =
            checkCoverage $
            cover 1 (isHorizon oh1) "horizon (meaningless test)" $
            cover 50 (isDistance (const True) oh1) "distance" $
            prop
          where oh1 = evaluateTerrain at
                prop = case addObst at oh1 of
                         Nothing -> True -- horizon (meaningless test)
                         Just newTerrain -> let oh2 = evaluateTerrain newTerrain in
                                            traceFailureWithTerrain at oh1 oh2
                                          $ distanceLargerOrEqual oh1
                                                                  (evaluateTerrain newTerrain)
                                                                $ fuzzedFactor / (max (abs $ cos angle) $ abs $ sin angle)


-- initial obstacle = o
-- extra obstacles = .
-- observer = @
--
-- When going from here (below) to there (below), distance decreases by at most 1 / (max (cos angle) $ sin angle)
--                                        .
--                   o                   .o.
--                                        .
--              @                    @
prop_distanceDecreasesWhenAdding4Obstacles :: ArbitraryTerrain -> Property
prop_distanceDecreasesWhenAdding4Obstacles
  = makePropertyDistanceDecreasesWhenAddingObstacles add4Obstacles 1.0


-- initial obstacle = o
-- extra obstacles = .
-- observer = @
--
-- When going from here (below) to there (below), distance decreases by at most 2 / (max (cos angle) $ sin angle)
--                                       ...
--                   o                   .o.
--                                       ...
--              @                    @
prop_distanceDecreasesWhenAdding8Obstacles :: ArbitraryTerrain -> Property
prop_distanceDecreasesWhenAdding8Obstacles
  = makePropertyDistanceDecreasesWhenAddingObstacles add8Obstacles 2.0


main :: IO ()
main = hspec $ do
  describe "add4Obstacles" $ do
     it "works on example" $
       add4Obstacles (ArbitraryTerrain {atRows = 3
                                       , atCols = 3
                                       , atAssocs = [(Tagged @Row 1, Tagged @Col 2)]
                                       , atX = 0.1
                                       , atY = 0.5
                                       , atAngle = 0.3})
                     (Obstacle 1.46545224215 2 1 2.56692925055)
       `shouldBe`
       (Just $ ArbitraryTerrain {atRows = 3
                                , atCols = 3
                                , atAssocs = [(Tagged @Row 1, Tagged @Col 2)
                                             ,(0, 2)
                                             ,(2, 2)
                                             ,(1, 1)]
                                , atX = 0.1
                                , atY = 0.5
                                , atAngle = 0.3})
  describe "lineOfFlight" $ do
    describe "Properties" $ do
      it "transposition" $ property $ prop_distanceConstantUnderSomeTransformation transpose
      it "vertical symmetry" $ property $ prop_distanceConstantUnderSomeTransformation verticalAxisSymmetry
      it "horizontal symmetry" $ property $ prop_distanceConstantUnderSomeTransformation horizontalAxisSymmetry
      it "analytical solution" $ property prop_analyticalDistanceCalculation
      it "distance to obstacles increases when removing them" $ property prop_distanceGrowsWhenRemovingObstacles
      it "distance to obstacles decreases when adding 4 of them" $ property prop_distanceDecreasesWhenAdding4Obstacles
      it "distance to obstacles decreases when adding 8 of them" $ property prop_distanceDecreasesWhenAdding8Obstacles
    describe "Counter-examples found during test suite debugging" $ do
      let ex1 = ArbitraryTerrain {atRows = Tagged 3
                                 , atCols = Tagged 3
                                 , atAssocs = [(Tagged @Row 0,Tagged @Col 0)
                                              ,(Tagged 0,Tagged 0)
                                              ,(Tagged 2,Tagged 0)
                                              ,(Tagged 0,Tagged 2)
                                              ,(Tagged 2,Tagged 2)
                                              ,(Tagged 0,Tagged 0)
                                              ,(Tagged 2,Tagged 0)]
                                 , atX = Tagged 1.0
                                 , atY = Tagged 0.9
                                 , atAngle = 3.8} in
        describe "ex1" $ do
          it "computes distance" $ evaluateTerrain ex1 `shouldBe` (Obstacle 0.6537465740348641 0 0 1.0170924318362453)
          it "computes distance with 1 obstacle more" $
            evaluateTerrain (ex1 {atAssocs = atAssocs ex1 ++ [(Tagged @Row 1, Tagged @Col 0)]})
            `equalObstacleHit`
            (Obstacle 0.632137054991 0 1 0.013221954744)
          it "computes distance with 2 obstacles more" $
            evaluateTerrain (ex1 {atAssocs = atAssocs ex1 ++ [(Tagged @Row 1, Tagged @Col 0), (0, 1)]})
            `equalObstacleHit`
            (Obstacle 0.632137054991 0 1 0.013221954744)
      let ex2 = ArbitraryTerrain {atRows = Tagged @Row 3
                                 , atCols = Tagged @Col 3
                                 , atAssocs = [(Tagged @Row 1,Tagged @Col 2)]
                                 , atX = Tagged @Col 0.1
                                 , atY = Tagged @Row 0.5
                                 , atAngle = 0.3}
          d2 = evaluateTerrain ex2
          expectedD2 = Obstacle 1.46545224215 2 1 2.56692925055
          ex2p1 = ex2 {atAssocs = [(Tagged @Row 1,Tagged @Col 1)] ++ atAssocs ex2}
          d2p1 = evaluateTerrain ex2p1
          expectedD2p1 = Obstacle 0.418700640615 1 1 2.87626550016 in
        -- ex2 =   0    1    2
        --     2
        --     1             #
        --     0   @
        describe "ex2" $ do
          it "computes distance" $ d2 `equalObstacleHit` expectedD2
          it "computes distance with 1 obstacle more" $ d2p1 `equalObstacleHit` expectedD2p1
      let ex3 = ArbitraryTerrain {atRows = 2
                                 , atCols = 6
                                 , atAssocs = [(Tagged @Row 1,Tagged @Col 1)
                                              ,(Tagged 1,Tagged 2)
                                              ,(Tagged 1,Tagged 3)
                                              ,(Tagged 1,Tagged 4)]
                                 , atX = -6.28442671097389e-2
                                 , atY = -0.17558135838450717
                                 , atAngle = 0.14697477611029086}
          d3 = evaluateTerrain ex3
          -- The ray passes close to a corner when computing with a tolerance of 1e-4.  With
          -- epsilon = 1e-6, the ray "misses" the obstacle.
          -- expectedD3 = Obstacle 4.613170990491703 4 1 4.0005904498241565
          expectedD3 = Horizon
          ex3p1 = ex3 {atAssocs = [(Tagged @Row 0,Tagged @Col 4)] ++ atAssocs ex3}
          d3p1 = evaluateTerrain ex3p1
          expectedD3p1 = Obstacle 3.6016752372242617 4 0 2.148129708697345 in
        describe "ex3" $ do
          it "computes distance" $ d3 `equalObstacleHit` expectedD3
          it "computes distance with 1 obstacle more" $ d3p1 `equalObstacleHit` expectedD3p1

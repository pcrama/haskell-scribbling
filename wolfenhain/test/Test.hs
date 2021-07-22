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


data ArbitraryTerrain = ArbitraryTerrain { atRows :: NonNegative Int
                                         , atCols :: NonNegative Int
                                         , atAssocs :: [(NonNegative Int, NonNegative Int)]
                                         , atX :: NonNegative Double
                                         , atY :: NonNegative Double
                                         , atAngle :: Double }
  deriving (Show, Eq)


buildTerrainArray :: ArbitraryTerrain -> R1st2D
buildTerrainArray ArbitraryTerrain { atRows = NonNegative rows
                                   , atCols = NonNegative cols
                                   , atAssocs = assocs }
  = rowFirst2DArray rows
                    cols
                    [(r, c, 1) | (NonNegative r, NonNegative c) <- assocs]


evaluateTerrain :: ArbitraryTerrain -> ObstacleHit
evaluateTerrain terrain@(ArbitraryTerrain { atX = NonNegative x
                                          , atY = NonNegative y
                                          , atAngle = angle })
  = lineOfFlight x y angle $ buildTerrainArray terrain


renderTerrain :: ArbitraryTerrain -> ObstacleHit -> ObstacleHit -> String
renderTerrain a@(ArbitraryTerrain { atRows = NonNegative rows
                                  , atCols = NonNegative cols
                                  , atX = NonNegative x
                                  , atY = NonNegative y })
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
    cover 10 (isHorizon d1) "horizon" $
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
        equalObstacle (Obstacle _ xh yh _) (Obstacle _ x' y' _) = fI yh xh == (y', x')
        equalObstacle (Obstacle _ _ _ _) Horizon = False
        equalObstacle Horizon (Obstacle _ _ _ _) = False


fmod :: Double -> Int -> Double
fmod x y = x - (fromIntegral $ (floor x `div` y) * y)


instance Arbitrary ArbitraryTerrain where
  arbitrary = do
    NonNegative arbRows <- arbitrary
    NonNegative arbCols <- arbitrary
    NonNegative arbX <- arbitrary
    NonNegative arbY <- arbitrary
    arbAngle <- arbitrary
    let rows = arbRows + 3 -- at least 3 rows
    let cols = arbCols + 3 -- at least 3 columns
    let x = arbX `fmod` cols
    let y = arbY `fmod` rows
    let intX = round x
    let intY = round y
    let angle = arbAngle `fmod` 13
    assocs <- oneof [return $ [p
                              | c <- [0..cols - 1]
                              , p <- [(NonNegative $ c `mod` 2, NonNegative c)
                                     , (NonNegative $ rows - c `mod` 2 - 1, NonNegative c)]]
                           ++ [p
                              | r <- [0..rows - 1]
                              , p <- [(NonNegative r, NonNegative $ r `mod` 2)
                                     , (NonNegative r, NonNegative $ cols - r `mod` 2 - 1)]]
                    , do
                        NonNegative arbThreshold <- arbitrary
                        let threshold = (arbThreshold `mod` 10 + 2)
                        return [(NonNegative r, NonNegative c)
                               | r <- [0..rows - 1]
                               , c <- [0..cols - 1]
                               , (r + c) `mod` 14 >= threshold]]
    return ArbitraryTerrain { atRows = NonNegative rows
                            , atCols = NonNegative cols
                            , atAssocs = [(NonNegative $ r `rem` rows, NonNegative $ c `rem` cols)
                                         | (NonNegative r, NonNegative c) <- assocs
                                         , r /= intY && c /= intX]
                            , atX = NonNegative x
                            , atY = NonNegative y
                            , atAngle = angle}
  shrink (ArbitraryTerrain { atAssocs = [] }) = []
  shrink at@(ArbitraryTerrain { atRows = NonNegative rows, atCols = NonNegative cols, atAssocs = assocs, atAngle = angle, atX = NonNegative x, atY = NonNegative y }) =
      shrinkRows ++ shrinkCols ++ shrinkWalls ++ shrinkAngle ++ shrinkRounding (x, y) (coordRounder 10) coordSetter ++ shrinkRounding angle (doubleRounder 10) angleSetter
    where shrinkRows
            | rows `mod` 2 == 0 = shrunkTop at ++ shrunkBottom at
            | otherwise = shrunkBottom at ++ shrunkTop at
          shrinkCols
            | cols `mod` 2 == 0 = shrunkLeft at ++ shrunkRight at
            | otherwise = shrunkRight at ++ shrunkLeft at
          shrinkWalls = do
                          shrunkAssocs <- shrinkHeadTail assocs ++ shrinkOddEvenGo assocs [] []
                          guard $ null shrunkAssocs
                          return $ at { atAssocs = shrunkAssocs }
          shrinkHeadTail [] = []
          shrinkHeadTail (z:zs) = [[z], zs]
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
          coordRounder factor (xf, yf) = (doubleRounder factor xf, doubleRounder factor yf)
          angleSetter a' = at { atAngle = a' }
          coordSetter (x', y') = at { atX = NonNegative x', atY = NonNegative y' }


shrunkTop, shrunkBottom, shrunkLeft, shrunkRight :: ArbitraryTerrain -> [ArbitraryTerrain]
shrunkTop (ArbitraryTerrain { atRows = NonNegative rows
                            , atCols = NonNegative cols
                            , atAssocs = assocs
                            , atX = x
                            , atY = NonNegative y
                            , atAngle = angle })
  | round y >= rows - 1 || rows <= 3 = []
  | otherwise = let newRows = rows - 1 in
                case [(r, c) | (r, c) <- assocs, r < NonNegative newRows] of
                  [] -> []
                  shrunkAssocs -> [ArbitraryTerrain { atRows = NonNegative newRows
                                                    , atCols = NonNegative cols
                                                    , atAssocs = shrunkAssocs
                                                    , atX = x
                                                    , atY = NonNegative y
                                                    , atAngle = angle }]


shrunkBottom (ArbitraryTerrain { atRows = NonNegative rows
                               , atCols = cols
                               , atAssocs = assocs
                               , atX = x
                               , atY = NonNegative y
                               , atAngle = angle })
  | round y <= (0 :: Int) || rows <= 3 = []
  | otherwise = let newRows = rows - 1 in
                case [(NonNegative $ r - 1, c) | (NonNegative r, c) <- assocs, r > 0] of
                  [] -> []
                  shrunkAssocs -> [ArbitraryTerrain { atRows = NonNegative newRows
                                                    , atCols = cols
                                                    , atAssocs = shrunkAssocs
                                                    , atX = x
                                                    , atY = NonNegative $ y - 1
                                                    , atAngle = angle }]


shrunkRight (ArbitraryTerrain { atRows = NonNegative rows
                              , atCols = NonNegative cols
                              , atAssocs = assocs
                              , atX = NonNegative x
                              , atY = y
                              , atAngle = angle })
  | round x >= cols - 1 || cols <= 3 = []
  | otherwise = let newCols = cols - 1 in
                case [(r, c) | (r, c) <- assocs, c < NonNegative newCols] of
                  [] -> []
                  shrunkAssocs -> [ArbitraryTerrain { atRows = NonNegative rows
                                                    , atCols = NonNegative newCols
                                                    , atAssocs = shrunkAssocs
                                                    , atX = NonNegative x
                                                    , atY = y
                                                    , atAngle = angle }]


shrunkLeft (ArbitraryTerrain { atRows = rows
                             , atCols = NonNegative cols
                             , atAssocs = assocs
                             , atX = NonNegative x
                             , atY = y
                             , atAngle = angle })
  | round x <= (0 :: Int) || cols <= 3 = []
  | otherwise = let newCols = cols - 1 in
                case [(r, NonNegative $ c - 1) | (r, NonNegative c) <- assocs, c > 0] of
                  [] -> []
                  shrunkAssocs -> [ArbitraryTerrain { atRows = rows
                                                    , atCols = NonNegative newCols
                                                    , atAssocs = shrunkAssocs
                                                    , atX = NonNegative $ x - 1
                                                    , atY = y
                                                    , atAngle = angle }]


transformSize :: (Int -> Int -> (Int, Int)) -- ^ transform row col to (row', col')
              -> Int -- ^ rows
              -> Int -- ^ cols
              -> (Int, Int) -- ^ size (rows', cols') of ([0..rows), [0..cols)) after transformation
transformSize f rows cols = (1 + max r1 r2, 1 + max c1 c2)
  where (r1, c1) = f 0 0
        (r2, c2) = f (rows - 1) (cols - 1)


data TerrainTransformationD = TerrainTransformationD (Int -> Int -> (Int, Int))
                                                     (Double -> Double -> (Double, Double))
                                                     (Double -> Double)


type TerrainTransformation = ArbitraryTerrain -> TerrainTransformationD


transpose, verticalAxisSymmetry, horizontalAxisSymmetry :: TerrainTransformation
transpose _ = TerrainTransformationD (\r c -> (c, r)) (\x y -> (y, x)) (\angle -> pi / 2.0 - angle)


-- | Symmetry around a vertical axis
verticalAxisSymmetry (ArbitraryTerrain { atCols = NonNegative cols })
    = TerrainTransformationD (\r c -> (r, cols - 1 - c))
                            (\x y -> (fromIntegral cols - 1 - x, y))
                            (\angle -> pi - angle)


-- | Symmetry around a horizontal axis
horizontalAxisSymmetry (ArbitraryTerrain { atRows = NonNegative rows })
    = TerrainTransformationD (\r c -> (rows - 1 - r, c))
                             (\x y -> (x, fromIntegral rows - 1 - y))
                             (\angle -> 0 - angle)


transformTerrain :: TerrainTransformation -> ArbitraryTerrain -> ArbitraryTerrain
transformTerrain xform
                 at@(ArbitraryTerrain { atCols = NonNegative cols
                                      , atRows = NonNegative rows
                                      , atAssocs = assocs
                                      , atX = NonNegative x
                                      , atY = NonNegative y
                                      , atAngle = angle })
  = at { atCols = NonNegative cols'
       , atRows = NonNegative rows'
       , atAssocs = map fN assocs
       , atX = NonNegative x'
       , atY = NonNegative y'
       , atAngle = fA angle
       }
    where (TerrainTransformationD fI fD fA) = xform at
          (rows', cols') = transformSize fI rows cols
          (x', y') = fD x y
          fN (NonNegative r, NonNegative c) = let (r', c') = fI r c in (NonNegative r', NonNegative c')


-- | Compute distance to next obstacle analytically, assuming the terrain is simply a closed rectangle
analyticalSolution :: Int -- ^ Rows
                   -> Int -- ^ Cols
                   -> Double -- ^ x
                   -> Double -- ^ y
                   -> Double -- ^ angle
                   -> Double
analyticalSolution rows cols x y angle =
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
prop_analyticalDistanceCalculation at@(ArbitraryTerrain { atRows = NonNegative rows
                                                        , atCols = NonNegative cols
                                                        , atX = NonNegative x
                                                        , atY = NonNegative y
                                                        , atAngle = angle }) =
    equalDistance oh $ Obstacle analyticalDistance undefined undefined undefined
  where oh = evaluateTerrain $ at { atAssocs = [(NonNegative r, NonNegative 0) | r <- [0..rows - 1]]
                                            ++ [(NonNegative r, NonNegative $ cols - 1) | r <- [0..rows - 1]]
                                            ++ [(NonNegative 0, NonNegative c) | c <- [0..cols - 1]]
                                            ++ [(NonNegative $ rows - 1, NonNegative c) | c <- [0..cols - 1]]
                                  , atX = NonNegative xMappedIntoTerrain
                                  , atY = NonNegative yMappedIntoTerrain }
        analyticalDistance = analyticalSolution rows cols xMappedIntoTerrain yMappedIntoTerrain angle
        xMappedIntoTerrain = 0.501 + (x `fmod` (fromIntegral $ cols - 2)) * 0.9
        yMappedIntoTerrain = 0.501 + (y `fmod` (fromIntegral $ rows - 2)) * 0.9


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
        obstacleNeighbour x y (NonNegative r, NonNegative c) = (abs $ r - y) <= 1 && (abs $ c - x) <= 1


addObstacles :: [(Int, Int)] -> ArbitraryTerrain -> ObstacleHit -> Maybe ArbitraryTerrain
addObstacles _ _ Horizon = Nothing
addObstacles rowColOffs
             at@(ArbitraryTerrain { atRows = NonNegative rows
                                  , atCols = NonNegative cols
                                  , atX = NonNegative x
                                  , atY = NonNegative y
                                  , atAssocs = assocs })
             (Obstacle _ c r _) = Just newTerrain
  where newTerrain = at { atAssocs = nub $ assocs
                                        ++ [(NonNegative r', NonNegative c')
                                           | (ro, co) <- rowColOffs
                                           , let r' = r + ro
                                           , r' >= 0 && r' < rows
                                           , let c' = c + co
                                           , c' >= 0 && c' < cols
                                           , not $ round y == r' && round x == c'] }


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
       add4Obstacles (ArbitraryTerrain {atRows = NonNegative {getNonNegative = 3}
                                       , atCols = NonNegative {getNonNegative = 3}
                                       , atAssocs = [(NonNegative {getNonNegative = 1},NonNegative {getNonNegative = 2})]
                                       , atX = NonNegative {getNonNegative = 0.1}
                                       , atY = NonNegative {getNonNegative = 0.5}
                                       , atAngle = 0.3})
                     (Obstacle 1.46545224215 2 1 2.56692925055)
       `shouldBe`
       (Just $ ArbitraryTerrain {atRows = NonNegative {getNonNegative = 3}
                                , atCols = NonNegative {getNonNegative = 3}
                                , atAssocs = [(NonNegative {getNonNegative = 1},NonNegative {getNonNegative = 2})
                                             ,(NonNegative {getNonNegative = 0},NonNegative {getNonNegative = 2})
                                             ,(NonNegative {getNonNegative = 2},NonNegative {getNonNegative = 2})
                                             ,(NonNegative {getNonNegative = 1},NonNegative {getNonNegative = 1})]
                                , atX = NonNegative {getNonNegative = 0.1}
                                , atY = NonNegative {getNonNegative = 0.5}
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
      let ex1 = ArbitraryTerrain {atRows = NonNegative {getNonNegative = 3}
                                 , atCols = NonNegative {getNonNegative = 3}
                                 , atAssocs = [(NonNegative {getNonNegative = 0},NonNegative {getNonNegative = 0})
                                              ,(NonNegative {getNonNegative = 0},NonNegative {getNonNegative = 0})
                                              ,(NonNegative {getNonNegative = 2},NonNegative {getNonNegative = 0})
                                              ,(NonNegative {getNonNegative = 0},NonNegative {getNonNegative = 2})
                                              ,(NonNegative {getNonNegative = 2},NonNegative {getNonNegative = 2})
                                              ,(NonNegative {getNonNegative = 0},NonNegative {getNonNegative = 0})
                                              ,(NonNegative {getNonNegative = 2},NonNegative {getNonNegative = 0})]
                                 , atX = NonNegative {getNonNegative = 1.0}
                                 , atY = NonNegative {getNonNegative = 0.9}
                                 , atAngle = 3.8} in
        describe "ex1" $ do
          it "computes distance" $ evaluateTerrain ex1 `shouldBe` (Obstacle 0.6537465740348641 0 0 1.0170924318362453)
          it "computes distance with 1 obstacle more" $
            evaluateTerrain (ex1 {atAssocs = atAssocs ex1 ++ [(NonNegative 1, NonNegative 0)]})
            `equalObstacleHit`
            (Obstacle 0.632137054991 0 1 0.013221954744)
          it "computes distance with 2 obstacles more" $
            evaluateTerrain (ex1 {atAssocs = atAssocs ex1 ++ [(NonNegative 1, NonNegative 0), (NonNegative 0, NonNegative 1)]})
            `equalObstacleHit`
            (Obstacle 0.632137054991 0 1 0.013221954744)
      let ex2 = ArbitraryTerrain {atRows = NonNegative {getNonNegative = 3}
                                 , atCols = NonNegative {getNonNegative = 3}
                                 , atAssocs = [(NonNegative {getNonNegative = 1},NonNegative {getNonNegative = 2})]
                                 , atX = NonNegative {getNonNegative = 0.1}
                                 , atY = NonNegative {getNonNegative = 0.5}
                                 , atAngle = 0.3}
          d2 = evaluateTerrain ex2
          expectedD2 = Obstacle 1.46545224215 2 1 2.56692925055
          ex2p1 = ex2 {atAssocs = [(NonNegative {getNonNegative = 1},NonNegative {getNonNegative = 1})]
                                ++ atAssocs ex2}
          d2p1 = evaluateTerrain ex2p1
          expectedD2p1 = Obstacle 0.418700640615 1 1 2.87626550016 in
        -- ex2 =   0    1    2
        --     2
        --     1             #
        --     0   @
        describe "ex2" $ do
          it "computes distance" $ d2 `equalObstacleHit` expectedD2
          it "computes distance with 1 obstacle more" $ d2p1 `equalObstacleHit` expectedD2p1

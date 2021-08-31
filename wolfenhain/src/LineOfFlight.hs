{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}

module LineOfFlight (
  ObstacleHit(..),
  lineOfFlight,
)

where

import Terrain

data ObstacleHit = Horizon -- ^ look to infinity
                 | Obstacle Double -- ^ distance
                            Int    -- ^ cell X
                            Int    -- ^ cell Y
                            Double -- ^ [0..4] how far along the wall
                                   --   0: South-East corner
                                   --   1: North-East corner
                                   --   2: North-West corner
                                   --   3: South-West corner
  deriving (Eq, Show)


lineOfFlight :: Double -- ^ X coordinate of player
             -> Double -- ^ Y coordinate of player
             -> Double -- ^ angle of ray (in radians, 0 means east, pi/2 means north)
             -> R1st2D -- ^ terrain map (0 means empty)
             -> ObstacleHit
lineOfFlight xp yp dir room =
    case (u >= 0, v >= 0) of
      (True, True) -> go right up
      (False, True) -> go left up
      (False, False) -> go left down
      (True, False) -> go right down
  where u = cos dir -- horizontal component of unity vector of ray
        v = sin dir -- vertical component of unity vector of ray
        {-# INLINE go #-}
        go !moveRight !moveUp = internalLineOfFlight xp yp room u v moveRight moveUp ix iy
        ix = round xp
        iy = round yp


type UpdatePosition a = a -> a -> a -> (a, a)


{-# INLINE up #-}
{-# INLINE right #-}
{-# INLINE down #-}
{-# INLINE left #-}
up, right, down, left :: UpdatePosition Int
up !x !y !s = (x, y + s)
right !x !y !s = (x + s, y)
down !x !y !s = (x, y - s)
left !x !y !s = (x - s, y)


{-# INLINE internalLineOfFlight #-}
internalLineOfFlight :: Double -- ^ X coordinate of player
                     -> Double -- ^ Y coordinate of player
                     -> R1st2D -- ^ terrain map (0 means empty)
                     -> Double -- ^ horizontal component of unity vector of ray
                     -> Double -- ^ vertical component of unity vector of ray
                     -> UpdatePosition Int -- ^ move in same east/west direction as ray
                     -> UpdatePosition Int -- ^ move in same north/south direction as ray
                     -> Int -- ^ X cell of player, then point along the way
                     -> Int -- ^ Y cell of player, then point along the way
                     -> ObstacleHit
internalLineOfFlight !xp !yp !room !u !v !rightI !upI !ix !iy
  | abs cornerDistanceFromRay < epsilon = tryRight $ const2 $ tryUp $ const2 $ tryDiagonalUp advanceAlongRay
  | abs upDistanceFromRay < epsilon = tryUp advanceAlongRay
  | sameSign upDistanceFromRay cornerDistanceFromRay = tryRight advanceAlongRay
  | abs rightDistanceFromRay < epsilon = tryRight advanceAlongRay
  | otherwise = tryUp advanceAlongRay
  where cols = colCount room
        rows = rowCount room
        diagUpRight, diagUpLeft, diagDownRight :: UpdatePosition Int
        diagUpRight !x !y !s = let (x', y') = upI x y s in rightI x' y' s
        diagUpLeft !x !y !s = let (x', y') = upI x y s in rightI x' y' $ 0 - s
        diagDownRight !x !y !s = let (x', y') = rightI x y s in upI x' y' $ 0 - s
        intMean !a !b = (fromIntegral $ a + b) / 2.0
        sameSign !a !b = signum a == signum b
        (!xc, !yc) = let (x', y') = diagUpRight ix iy 1 in (intMean ix x', intMean iy y')
        (!xu, !yu) = let (x', y') = diagUpLeft ix iy 1 in (intMean ix x', intMean iy y')
        (!xr, !yr) = let (x', y') = diagDownRight ix iy 1 in (intMean ix x', intMean iy y')
        distanceFromRay !x !y = u * (y - yp) + v * (xp - x)
        epsilon = 0.000001
        cornerDistanceFromRay = distanceFromRay xc yc
        upDistanceFromRay = distanceFromRay xu yu
        rightDistanceFromRay = distanceFromRay xr yr
        const2 :: ObstacleHit -> (Int -> Int -> ObstacleHit)
        const2 !f _ _ = f
        isWall !x !y = r12d room y x /= 0
        advanceAlongRay :: Int -> Int -> ObstacleHit
        advanceAlongRay = internalLineOfFlight xp yp room u v rightI upI
        stepAndTry :: UpdatePosition Int -- ^ move to try, aborting if exiting the terrain map
                   -> ((Int -> Int -> ObstacleHit) -> Int -> Int -> ObstacleHit) -- ^ look for obstacle in destination
                   -> (Int -> Int -> ObstacleHit)
                   -> ObstacleHit
        stepAndTry !step !attempt !nextStep = case step ix iy 1 of
          (jx, jy) | jx < 0 || jy < 0 -> Horizon
                   | jx >= cols -> Horizon
                   | jy >= rows -> Horizon
                   | otherwise -> attempt nextStep jx jy
        tryDiagonalUp, tryRight, tryUp :: (Int -> Int -> ObstacleHit) -> ObstacleHit
        tryDiagonalUp', tryRight', tryUp' :: (Int -> Int -> ObstacleHit) -> Int -> Int -> ObstacleHit
        tryDiagonalUp = stepAndTry diagUpRight tryDiagonalUp'
        tryDiagonalUp' !nextStep !jx !jy
          -- hit a corner
          | isWall jx jy =
            let distance = if u > v then (xc - xp) / u else (yc - yp) / v in
            Obstacle distance jx jy $ case (xc > fromIntegral jx, yc > fromIntegral jy) of
                                        (True, False) -> 0
                                        (True, True) -> 1
                                        (False, True) -> 2
                                        (False, False) -> 3
          | otherwise = nextStep jx jy
        tryRight = stepAndTry rightI tryRight'
        tryRight' !nextStep !jx !jy =
          let northY = fromIntegral jy + 0.5
              southY = fromIntegral jy - 0.5
              tapestry y
                -- looking right, seeing western wall: north=2, south=3
                | u > 0 = 2 + northY - y
                -- looking left, seeing eastern wall: south=0, north=1
                | otherwise = y - southY
              obstacle x d = Obstacle d jx jy $ tapestry x in
          case (isWall jx jy, abs u < abs v / 100) of
            -- no obstacle
            (False, _) -> nextStep jx jy
            -- intersection with vertical line, 2 cases
            (True, False) ->
              -- 1. Simple case, ray is not nearly parallel with wall
              let d = (xc - xp) / u
                  yWall = v * d + yp in
                obstacle yWall d
            (True, True) ->
              -- 2. Complicated case, ray is nearly parallel with wall
              -- Avoid numerical instability: ray is nearly vertical, so
              -- xWall \approx xp, and (xWall - xp) / u is small divided
              -- by small.  Use an approximation, based on distance of
              -- wall's corners from the ray.
              let northDistance = abs $ distanceFromRay xc northY
                  southDistance = abs $ distanceFromRay xc southY
                  approxY = southY + (northY - southY) * southDistance / (northDistance + southDistance) in
                obstacle approxY $ (approxY - yp) / v
        tryUp = stepAndTry upI tryUp'
        tryUp' !nextStep !jx !jy =
          let westX = fromIntegral jx - 0.5
              eastX = fromIntegral jx + 0.5
              tapestry x
                -- looking up, seeing southern wall: west=3, east=4
                | v > 0 = 3 + x - westX
                -- looking down, seeing northern wall: west=2, east=1
                | otherwise = 1 + eastX - x
              obstacle x d = Obstacle d jx jy $ tapestry x in
          case (isWall jx jy, abs v < abs u / 100) of
            (False, _) -> nextStep jx jy
            -- intersection with horizontal line, 2 cases
            (True, False) ->
              -- 1. Simple case, ray is not nearly parallel with wall
              let d = (yc - yp) / v
                  xWall = u * d + xp in
                obstacle xWall d
            (True, True) ->
              -- 2. Complicated case, ray is nearly parallel with wall
              -- Avoid numerical instability: ray is nearly horizontal, so
              -- yWall \approx yp, and (yWall - yp) / v is small divided
              -- by small.  Use an approximation, based on distance of
              -- wall's corners from the ray.
              let westDistance = abs $ distanceFromRay westX yc
                  eastDistance = abs $ distanceFromRay eastX yc
                  approxX = eastX + (westX - eastX) * eastDistance / (westDistance + eastDistance) in
                obstacle approxX $ (approxX - xp) / u

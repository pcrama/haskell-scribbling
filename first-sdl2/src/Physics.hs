module Physics (
  GameAcceleration(..)
  , GameSpeed(..)
  , GameTime
  , Jump(..)
  , MUA(..)
  , Position
  , jumpDistance
  , jumpDuration
  , jumpEndTime
  , jumpPosition
  , muaDistance
  , muaMaxDistance
  , muaSpeed
  , muaTimeOfMaxDistance
  , speedZero
  , timeScaling
  )
where

import Data.Word (Word32)
import Foreign.C.Types (CInt)


type Position = CInt -- pixels, whatever type sdl2 wants


-- | Acceleration newtype wrapper, in pixels/s^2
newtype GameAcceleration = GA Double
  deriving (Show)

-- | Speed newtype wrapper, in pixels/s
newtype GameSpeed = GS Double
  deriving (Ord, Eq, Show)

-- | Convenience constant for comparisons
speedZero :: GameSpeed
speedZero = GS 0.0

-- | Timestamp in ms
type GameTime = Word32 -- whatever sdl2 wants

-- | MUA stands for Movement with Uniform Acceleration
-- For the formulas of muaMaxDistance and muaTimeOfMaxDistance to be
-- correct, acceleration and initial speed should have opposing signs.
data MUA = MUA {
  muaA :: !GameAcceleration -- ^ Constant acceleration
  , muaT0 :: !GameTime -- ^ Start time
  , muaV :: !GameSpeed -- ^ Start Speed
  , muaX0 :: !Position -- ^ Initial position
  }

-- | Conversion factor from GameTime to seconds
timeScaling :: Double
timeScaling = 1000.0 -- milliseconds in a second

-- | Convert time difference into seconds
timeDiff :: GameTime -> GameTime -> Double
timeDiff t t0 = fromIntegral (t - t0) / timeScaling

muaSpeed :: MUA -> GameTime -> GameSpeed
muaSpeed (MUA { muaA=(GA a), muaV=(GS v), muaT0=t0 }) t =
  GS $ v + a * (t `timeDiff` t0)

muaDistance :: MUA -> GameTime -> Position
muaDistance (MUA { muaA=(GA a)
                 , muaV=(GS v)
                 , muaT0=t0 })
            t =
  let t_t0 = t `timeDiff` t0 in
  round $ (a * t_t0 / 2 + v) * t_t0

-- | When the speed changes sign
-- Note that the result is positive only if acceleration and initial
-- speed have opposing signs.
muaTimeOfMaxDistance :: MUA -> GameTime
muaTimeOfMaxDistance (MUA { muaT0=t0, muaA=GA a, muaV=GS v }) = t0 + round (timeScaling * v / (0 - a))

-- | The farthest the movement goes before speed changes sign
-- Note that the result is positive only if acceleration and initial
-- speed have opposing signs.
muaMaxDistance :: MUA -> Position
-- (a * (-v / a) / 2 + v) * (-v / a) = -1/2 * v^2 / a
muaMaxDistance (MUA { muaA=GA a, muaV=GS v }) = round $ (-0.5) * v * v / a


-- | Jumping hero: constant horizontal speed, MUA for vertical movement
data Jump = Jump {
  jumpX0 :: !Position -- | ^ starting x position
  , jumpVx0 :: !GameSpeed -- | ^ initial horizontal speed
  , jumpYMvt :: !MUA -- | ^ movement along vertical axis (also contains starting time)
  }


jumpHeight :: Jump -> GameTime -> Position
jumpHeight (Jump {jumpYMvt=mua}) t = muaDistance mua t


jumpDistance :: Jump -> GameTime -> Position
jumpDistance (Jump {jumpYMvt=mua, jumpVx0=GS v}) t = round $ v * (t `timeDiff` muaT0 mua)


-- | How many seconds the hero will be jumping before landing
jumpDuration :: Jump -> Double
jumpDuration (Jump {jumpYMvt=mua}) = 0 - (v + (sqrt $ v * v - 2 * a * x0)) / a
  -- solve x0 + a/2 * t^2 + v * t = 0 for t
  -- (take larger of the 2 and assume that a < 0 because of gravity)
  -- t = (-v - sqrt (v^2 - 2 * a * x0)) / a
  where GA a = muaA mua
        GS v = muaV mua
        x0 = fromIntegral $ muaX0 mua


-- | How far the hero will travel horizontally before landing on the ground
maxJumpDistance :: Jump -> Position
maxJumpDistance j@(Jump {jumpVx0=GS v}) = round $ v * jumpDuration j


-- | When the hero will land
jumpEndTime :: Jump -> GameTime
jumpEndTime j = (muaT0 $ jumpYMvt j) + (round $ timeScaling * jumpDuration j)


-- | Position of hero during jump as (x, y) tuple
jumpPosition :: Jump -> GameTime -> (Position, Position)
jumpPosition j t = (x, y)
  where y' = (muaX0 $ jumpYMvt j) + jumpHeight j t -- theoretical y position without collision detection
        y = max 0 y' -- can't fall through the floor
        x = jumpX0 j + if y' < 0
                       then maxJumpDistance j -- once back on the floor, movement stops
                       else jumpDistance j t

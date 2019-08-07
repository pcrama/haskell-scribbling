module Physics (
  GameAcceleration(..)
  , GameSpeed(..)
  , GameTime
  , MUA(..)
  , Position
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

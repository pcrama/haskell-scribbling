module Physics (
  GameAcceleration(..)
  , GameSpeed(..)
  , GameTime
  , MUA(..)
  , Position
  , muaDistance
  , muaSpeed
  , speedZero
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
data MUA = MUA {
  muaA :: !GameAcceleration -- ^ Constant acceleration
  , muaT0 :: !GameTime -- ^ Start time
  , muaV :: !GameSpeed -- ^ Start Speed
  , muaX0 :: !Position -- ^ Initial position
  }

-- | Convert time difference into seconds
timeDiff :: GameTime -> GameTime -> Double
timeDiff t t0 = fromIntegral (t - t0) / 1000.0

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

module Hero(
  Hero(..)
  , HeroDrawingInfo
  , HeroTextures(..)
  , heroDrawInfo
  , heroPosition
) where

import qualified SDL

import AllSDL
import Physics


data Hero =
  IdleHero GameTime Position
  | RunningHero MUA
  | JumpingHero Jump


data HeroTextures = HeroTextures {
    _heroTexture :: SDL.Texture
  , _heroIdleTexture :: SDL.Texture
  , _heroJumpTexture :: SDL.Texture
}


heroPosition :: GameTime -> Hero -> Position
heroPosition _   (IdleHero _ x0)    = x0
heroPosition now (RunningHero mua)  = muaX0 mua + muaDistance mua now
heroPosition now (JumpingHero jump) = fst $ jumpPosition jump now


type HeroDrawingInfo = (SDL.Texture, Int, Position, Position)


heroDrawInfo :: GameTime -> Hero -> HeroTextures -> Position -> HeroDrawingInfo
heroDrawInfo now (IdleHero t0 x0) context baseLine =
    (_heroIdleTexture context
    , (round $ 4 * (now `timeDiff` t0)) `mod` 8
    , x0 - heroWidth `div` 4
    , baseLine)
heroDrawInfo now (RunningHero mua) context baseLine =
  let frameCount = 3
      distance = muaDistance mua now
      GS speed = muaSpeed mua now in
    (_heroTexture context
     -- switch from speed based animation to time based to maintain illusion
     -- of movement at low speeds
    , if speed > 5
      then fromIntegral distance `mod` frameCount
      else (round $ 5 * (now `timeDiff` muaT0 mua)) `mod` frameCount
    , muaX0 mua + distance - heroWidth `div` 4
    , baseLine)
heroDrawInfo now (JumpingHero jump) context baseLine =
  let step = round $ 6 * (now `timeDiff` (muaT0 $ jumpYMvt jump))
      frameCount = 5
      (x, y) = jumpPosition jump now in
    (_heroJumpTexture context
    , abs $ (step `mod` (frameCount * 2 - 2)) - (frameCount - 1)
    , x - heroWidth `div` 4
    , baseLine - y)

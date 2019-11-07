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


-- | Texture, frame index into texture, x & y positions where to draw,
--   bounding box of character on screen
type HeroDrawingInfo = (SDL.Texture, Int, SDL.Point SDL.V2 Position, SDL.Rectangle Position)


heroDrawInfo :: GameTime -> Hero -> HeroTextures -> Position -> HeroDrawingInfo
heroDrawInfo now (IdleHero t0 x0) context baseLine =
    (_heroIdleTexture context, frameIdx, SDL.P drawXY, bbox)
  where frameIdx = (round $ 4 * (now `timeDiff` t0)) `mod` 8
        drawXY = SDL.V2 (x0 - heroWidth `div` 4) baseLine
        bbox = mkBbox drawXY $ case frameIdx of
          0 -> (17, 34, 27, 30)
          1 -> (81 - 1 * tileWidth, 36, 26, 28)
          2 -> (145 - 2 * tileWidth, 34, 27, 30)
          3 -> (209 - 3 * tileWidth, 34, 28, 30)
          4 -> (273 - 4 * tileWidth, 34, 27, 30)
          5 -> (337 - 5 * tileWidth, 36, 26, 28)
          6 -> (401 - 6 * tileWidth, 34, 27, 30)
          -- Exhaustiveness checker can't figure out that through the
          -- arithmetics of frameIdx, 7 is the only value that should be
          -- here:
          _ -> (465 - 7 * tileWidth, 34, 28, 30)
heroDrawInfo now (RunningHero mua) context baseLine =
  let distance = muaDistance mua now
      frameIdx = (if speed > 5
                  then fromIntegral distance
                  else (round $ 5 * (now `timeDiff` muaT0 mua))
                 ) `mod` 3 -- frames
      drawXY = SDL.V2 (muaX0 mua + distance - heroWidth `div` 4) baseLine
      GS speed = muaSpeed mua now in
    (_heroTexture context
     -- switch from speed based animation to time based to maintain illusion
     -- of movement at low speeds
    , frameIdx
    , SDL.P drawXY
    , mkBbox drawXY $ case frameIdx of
        0 -> (17, 33, 27, 30)
        1 -> (81 - 1 * tileWidth, 36, 27, 28)
        -- Exhaustiveness checker can't figure out that through the
        -- arithmetics of frameIdx, 2 is the only value that should be
        -- here:
        _ -> (145 - 2 * tileWidth, 33, 26, 31))
heroDrawInfo now (JumpingHero jump) context baseLine =
  let step = round $ 6 * (now `timeDiff` (muaT0 $ jumpYMvt jump))
      frameCount = 5
      (x, y) = jumpPosition jump now
      frameIdx = abs $ (step `mod` (frameCount * 2 - 2)) - (frameCount - 1)
      drawXY = SDL.V2 (x - heroWidth `div` 4) (baseLine - y)
      bbox = mkBbox drawXY $ case frameIdx of
        0 -> (17, 36, 27, 28)
        1 -> (81 - 1 * tileWidth, 29, 26, 33)
        2 -> (145 - 2 * tileWidth, 26, 27, 31)
        3 -> (209 - 3 * tileWidth, 25, 28, 30)
        -- Exhaustiveness checker can't figure out that through the
        -- arithmetics of frameIdx, 4 is the only value that should be
        -- here:
        _ -> (273 - 4 * tileWidth, 26, 28, 28) in
    (_heroJumpTexture context, frameIdx, SDL.P drawXY, bbox)


-- | Make scaled & shifted rectangle for bounding box of drawing inside the tile
--
--   The tiles are larger than the hero, so for an accurate collision
--   detection, we need to give a more precise bounding box inside the
--   tile.  Since the tile is scaled (cf heroWidth and tileWidth), the
--   bounding box needs the same scaling.  Note that the origin of the
--   tile is in absolute coordinates and doesn't need to be scaled.
mkBbox :: SDL.V2 Position -- ^ Tile's origin
       -> (Position, Position, Position, Position) -- ^ (x, y, w, h) relative to tile origin
       -> SDL.Rectangle Position -- ^ bounding box around drawn pixels
mkBbox org (x, y, w, h) =
  let s = heroWidth `div` tileWidth in
    moveRectangle (SDL.Rectangle (SDL.P $ SDL.V2 (s * x) (s * y))
                               $ SDL.V2 (s * w) (s * h))
                  org

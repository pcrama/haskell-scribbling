module Snake (
  Snake(..)
  , SnakeDrawingInfo
  , SnakeTextures(..)
  , drawSnake
  , killableSnakes
  , snakeDrawInfo
  , snakeInKillablePosition
  , snakePosition
) where

import qualified SDL
import qualified SDL.Font

import Control.Monad.IO.Class (MonadIO)

import AllSDL
import AtLeast2 (AtLeast2)
import Physics

data Snake =
  MovingSnake Position GameTime (AtLeast2 Char) -- initial position, `kill' word (ideally, it should be Word2 type synonym from FirstSdl2.hs)
  | DyingSnake Position GameTime -- static position, time at which snake should disappear

data SnakeTextures = SnakeTextures {
   _snakeTexture :: SDL.Texture
   , _snakeDieTexture :: SDL.Texture
}

snakeInKillablePosition :: Position -- ^ snake's position along horizontal axis
                        -> Position -- ^ hero's position along horizontal axis
                        -> Bool
snakeInKillablePosition snakePos heroPos = snakePos < heroPos + heroWidth `div` 2


snakePosition :: Snake -> GameTime -> Position
snakePosition (MovingSnake x0 t0 _) now
  | now > t0 = x0 - (round $ 5 * (now `timeDiff` t0))
  | otherwise = x0
snakePosition (DyingSnake x _) _ = x


killableSnakes :: GameTime -- ^ current time
               -> Position -- ^ scene origin (to filter out snakes based on visibility)
               -> Position -- ^ hero's position
               -> [Snake] -- ^ snakes
               -> [(Position, AtLeast2 Char)] -- ^ snakes that can be killed, identified by their init position
killableSnakes _ _ _ [] = []
killableSnakes now sceneOrigin heroPos (s:ss)
  | snakePos `snakeInKillablePosition` heroPos = processTail
  | snakePos > sceneOrigin + winWidth = []
  | otherwise = case s of
                  MovingSnake x0 _ w -> (x0, w):processTail
                  DyingSnake _ _ -> processTail
  where snakePos = snakePosition s now
        processTail = killableSnakes now sceneOrigin heroPos ss


type SnakeDrawingInfo = (Snake, SDL.Texture, Int, SDL.Point SDL.V2 Position, SDL.Rectangle Position) -- (snake, texture, frame, x & y where to draw, bounding box on screen)


snakeDrawInfo :: GameTime -- ^ current time
              -> Position -- ^ scene origin (to filter out snakes based on visibility)
              -> [Snake] -- ^ snakes
              -> SnakeTextures -- ^ snake images
              -> [SnakeDrawingInfo] -- ^ list of (snake, texture, frame, x & y, bbox)
snakeDrawInfo _ _ [] _ = []
snakeDrawInfo now sceneOrigin (s:ss) context
  | snakePos < sceneOrigin - snakeWidth = snakeDrawInfo now sceneOrigin ss context
  | snakePos > sceneOrigin + winWidth = []
  | otherwise = (completeTuple $ oneSnakeDrawInfo s):snakeDrawInfo now sceneOrigin ss context
  where snakePos = snakePosition s now
        snakeXY = SDL.V2 snakePos $ winHeight `div` 2 + heroHeight - snakeHeight
        completeTuple (t, f, bbox) = (s, t, f, SDL.P snakeXY, bbox)
        oneSnakeDrawInfo (DyingSnake _ timeout) =
          let frameIdx = (round $ (fromIntegral $ timeout - now)
                                  * 4 / timeScaling
                         ) `mod` 2
              bbox = mkBbox snakeXY $ if frameIdx == 0
                                      then (21, 37, 24, 27)
                                      else (85 - tileWidth, 36, 24, 28) in
            (_snakeDieTexture context, frameIdx, bbox )
        oneSnakeDrawInfo (MovingSnake _ _ _) =
          let frameIdx = fromIntegral $ snakePos `mod` 7
              bbox = mkBbox snakeXY $ case frameIdx of
                0 -> (19 - 0 * tileWidth, 43, 26, 21)
                1 -> (87 - 1 * tileWidth, 40, 25, 24)
                2 -> (152 - 2 * tileWidth, 39, 25, 25)
                3 -> (196 - 3 * tileWidth, 47, 51, 17)
                4 -> (260 - 4 * tileWidth, 47, 54, 17)
                5 -> (324 - 5 * tileWidth, 47, 49, 17)
                -- Exhaustiveness checker can't figure out that
                -- through the arithmetics of frameIdx, 6 is the only
                -- value that should be here:
                _ -> (391 - 6 * tileWidth, 44, 47, 20) in
            (_snakeTexture context, frameIdx, bbox)


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
  let s = snakeWidth `div` tileWidth in
    moveRectangle (SDL.Rectangle (SDL.P $ SDL.V2 (s * x) (s * y))
                               $ SDL.V2 (s * w) (s * h))
                  org


drawSnake :: MonadIO m
          => SDL.Renderer -- ^ SDL renderer
          -> Position -- ^ Scene origin
          -> SDL.Font.Font -- ^ Font for drawing `kill' word above snake
          -> Position -- ^ hero position
          -> SnakeDrawingInfo -- ^ see snakeDrawInfo
          -> m ()
drawSnake renderer sceneOrigin font heroPos (snake, texture, frame, SDL.P (SDL.V2 x y), bbox) = do
  case snake of
    MovingSnake _ _ toKill -- draw `kill' word
      | x `snakeInKillablePosition` heroPos -> return () -- can't shoot backwards or if snake is too close
      | otherwise ->
          withAtLeast2Texture renderer font black toKill $ \text -> do
            SDL.TextureInfo { SDL.textureWidth = textWidth
                            , SDL.textureHeight = textHeight
                            } <- SDL.queryTexture text
            lSDLcopy renderer
                     text
                     Nothing -- use complete texture as source
                   $ Just $ SDL.Rectangle (SDL.P $ SDL.V2 (x - sceneOrigin + (snakeWidth - textWidth) `div` 2)
                                                        $ y + snakeHeight)
                                        $ SDL.V2 textWidth textHeight
    DyingSnake _ _ -> return ()
  SDL.copy renderer
           texture
           (Just $ SDL.Rectangle (SDL.P $ SDL.V2 (fromIntegral frame * tileWidth) 0) $ SDL.V2 tileWidth tileHeight)
         $ Just $ SDL.Rectangle (SDL.P $ SDL.V2 (x - sceneOrigin) y) $ SDL.V2 snakeWidth snakeHeight
  let (SDL.Rectangle (SDL.P (SDL.V2 bbX bbY)) dim) = bbox in
    drawRectangle renderer (SDL.Rectangle (SDL.P (SDL.V2 (bbX - sceneOrigin) bbY)) dim)

module Snake (
  Snake(..)
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
import Data.List.NonEmpty     (NonEmpty)

import AllSDL
import Physics

data Snake =
  MovingSnake Position GameTime (NonEmpty Char) -- initial position, `kill' word
  | DyingSnake Position GameTime -- static position, time at which snake should disappear

data SnakeTextures = SnakeTextures {
   _snakeTexture :: SDL.Texture
   , _snakeDieTexture :: SDL.Texture
}

snakeInKillablePosition :: Position -- ^ snake's position
                        -> Position -- ^ hero's position
                        -> Bool
snakeInKillablePosition snakePos heroPos = snakePos - 64 - 32 < heroPos


snakePosition :: Snake -> GameTime -> Position
snakePosition (MovingSnake x0 t0 _) now
  | now > t0 = x0 - (round $ 5 * (now `timeDiff` t0))
  | otherwise = x0
snakePosition (DyingSnake x _) _ = x


killableSnakes :: GameTime -- ^ current time
               -> Position -- ^ scene origin (to filter out snakes based on visibility)
               -> Position -- ^ hero's position
               -> [Snake] -- ^ snakes
               -> [(Position, NonEmpty Char)] -- ^ snakes that can be killed, identified by their init position
killableSnakes _ _ _ [] = []
killableSnakes now sceneOrigin heroPos (s:ss)
  | snakePos `snakeInKillablePosition` heroPos = processTail
  | snakePos > sceneOrigin + winWidth = []
  | otherwise = case s of
                  MovingSnake x0 _ w -> (x0, w):processTail
                  DyingSnake _ _ -> processTail
  where snakePos = snakePosition s now
        processTail = killableSnakes now sceneOrigin heroPos ss


snakeDrawInfo :: GameTime -- ^ current time
              -> Position -- ^ scene origin (to filter out snakes based on visibility)
              -> [Snake] -- ^ snakes
              -> SnakeTextures -- ^ snake images
              -> [(Snake, SDL.Texture, Int, Position, Position)] -- ^ (snake, texture, frame, x, y)
snakeDrawInfo _ _ [] _ = []
snakeDrawInfo now sceneOrigin (s:ss) context
  | snakePos < sceneOrigin - 64 = snakeDrawInfo now sceneOrigin ss context
  | snakePos > sceneOrigin + winWidth = []
  | otherwise = oneSnakeDrawInfo s:snakeDrawInfo now sceneOrigin ss context
  where snakePos = snakePosition s now
        snakeY = winHeight `div` 2 + 128 - 64
        oneSnakeDrawInfo (DyingSnake _ timeout) = (s
                                                  , _snakeDieTexture context
                                                  , (round $ (fromIntegral $ timeout - now)
                                                             * 4 / timeScaling
                                                     ) `mod` 2
                                                  , snakePos
                                                  , snakeY)
        oneSnakeDrawInfo (MovingSnake _ _ _) = (s
                                               , _snakeTexture context
                                               , fromIntegral $ snakePos `mod` 6
                                               , snakePos
                                               , snakeY)


drawSnake :: MonadIO m
          => SDL.Renderer -- ^ SDL renderer
          -> Position -- ^ Scene origin
          -> SDL.Font.Font -- ^ Font for drawing `kill' word above snake
          -> Position -- ^ hero position
          -> (Snake, SDL.Texture, Int, Position, Position) -- ^ see snakeDrawInfo
          -> m ()
drawSnake renderer sceneOrigin font heroPos (snake, texture, frame, x, y) = do
  case snake of
    MovingSnake _ _ toKill -- draw `kill' word
      | x `snakeInKillablePosition` heroPos -> return () -- can't shoot backwards or if snake is too close
      | otherwise ->
          withNonEmptyTexture renderer font black toKill $ \text -> do
            SDL.TextureInfo { SDL.textureWidth = textWidth
                            , SDL.textureHeight = textHeight
                            } <- SDL.queryTexture text
            lSDLcopy renderer
                     text
                     Nothing -- use complete texture as source
                   $ Just $ SDL.Rectangle (SDL.P $ SDL.V2 (x - sceneOrigin + (64 - textWidth) `div` 2)
                                                        $ y + 64 + 16)
                                        $ SDL.V2 textWidth textHeight
    DyingSnake _ _ -> return ()
  lSDLcopy renderer
           texture
           (Just $ SDL.Rectangle (SDL.P $ SDL.V2 (fromIntegral frame * 64) 0) $ SDL.V2 64 64)
         $ Just $ SDL.Rectangle (SDL.P $ SDL.V2 (x - sceneOrigin) y) $ SDL.V2 64 64

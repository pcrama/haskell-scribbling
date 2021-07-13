{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (
  main
)

where

import Control.Concurrent     (threadDelay)
import Control.Exception      (handle, throw)
import Control.Monad          (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List              (foldl')
import System.Environment     (getArgs)

import qualified SDL
import qualified SDL.Font
import qualified SDL.Video.Renderer

import AllSDL
import Physics
import Terrain
import LineOfFlight

data AppContext = AppContext {
  _renderer :: SDL.Renderer
  , _font :: SDL.Font.Font
  , _room :: R1st2D
}


data AppState = AppState {
  _heroX :: Double
  , _heroY :: Double
  , _heroDir :: Double
  , _lastTicks :: GameTime
  , _fpsEst :: Double
}


win :: MonadIO m => R1st2D -> SDL.Font.Font -> SDL.Window -> m ()
win room font w = do
    renderer <- liftIO $ handle ((\e -> do
                                    backupRendererIndex <- getSoftwareRendererIndex
                                    case backupRendererIndex of
                                      Just i -> putStrLn (show e ++ ", retrying with " ++ show i) >> mkRenderer i
                                      Nothing -> putStrLn "Can't find backup driver" >> throw e
                                 ) :: SDL.SDLException -> IO SDL.Renderer)
                              $ mkRenderer (-1)
    now <- SDL.ticks
    let cols = colCount room
    let rows = rowCount room
    appLoop (AppState { _heroX = fromIntegral cols / 2.0, _heroY = fromIntegral rows / 2.0, _heroDir = 0.0, _lastTicks = now, _fpsEst = 0.0 })
            (AppContext { _renderer = renderer, _font = font, _room = room })
  where mkRenderer c = SDL.createRenderer w c SDL.defaultRenderer


updateAppForEvent :: SDL.Event -> AppContext -> AppState -> Maybe AppState
updateAppForEvent (SDL.Event _t SDL.QuitEvent) _ _ = Nothing
updateAppForEvent e (AppContext { _room = room }) s0
  | eventIsPress SDL.KeycodeEscape e = Nothing
  | eventIsPress SDL.KeycodeLeft e = Just $ s0 { _heroDir = _heroDir s0 + 0.15 }
  | eventIsPress SDL.KeycodeRight e = Just $ s0 { _heroDir = _heroDir s0 - 0.15 }
  | eventIsPress SDL.KeycodeUp e = tryToMove 1
  | eventIsPress SDL.KeycodeDown e = tryToMove $ -1
  | otherwise = noChange
  where noChange = Just s0
        hz = 0.125 * (cos $ _heroDir s0)
        vt = 0.125 * (sin $ _heroDir s0)
        tryToMove mult =
          let newX = _heroX s0 + mult * hz
              newY = _heroY s0 + mult * vt
              x' = round newX
              y' = round newY in
            if (newX < 0 || x' >= colCount room || newY < 0 || y' >= rowCount room
                || (((room `r12d` y') x') /= 0))
            then Just s0
            else Just $ s0 { _heroX = newX, _heroY = newY }


updateAppTime :: Monad m
              => GameTime -- ^ time
              -> AppState -- ^ current application state
              -> m (Maybe AppState -- ^ new state, Nothing means the game is over
                   )
updateAppTime now
              s0@(AppState { _lastTicks=t0, _fpsEst=fps0 })
  | now > t0 = do
      return $ Just $ s0 { _lastTicks=now
                         , _fpsEst=(pastWeight * fps0 + timeScaling / (fromIntegral $ now - t0))
                                     / (pastWeight + 1)
                         }
  | otherwise = return $ Just s0
  where pastWeight = 9 -- higher values mean more weight of the past FPS estimates in current estimate


appLoop :: MonadIO m => AppState -> AppContext -> m ()
appLoop oldState context = do
  events <- SDL.pollEvents
  case foldl' (\mbS e -> mbS >>= updateAppForEvent e context) (Just oldState) events of
    Nothing -> return ()
    Just s1 -> do
      now <- SDL.ticks
      mbNewState <- updateAppTime now s1
      case mbNewState of
        Nothing -> return ()
        Just nextState -> do
          drawApp nextState context
          when (_fpsEst nextState > 100) $
            liftIO $ threadDelay $ 10 * 1000 -- microseconds
          appLoop nextState context


-- | Draw application state
drawApp :: MonadIO m
        => AppState -- ^ current application state
        -> AppContext -- ^ application graphic context
        -> m ()
drawApp s (AppContext { _renderer=renderer, _font=font, _room = room }) = do
  SDL.rendererDrawColor renderer SDL.$= skyBlue
  SDL.clear renderer
  SDL.rendererDrawColor renderer SDL.$= green
  SDL.Video.Renderer.fillRect renderer
                            $ Just $ SDL.Rectangle (SDL.P $ SDL.V2 0 horizont)
                                                 $ SDL.V2 winWidth $ winHeight - horizont
  let from = -winWidth `div` 2
  let to = winWidth + from - 1
  let maxAngleTangent = 1.25
  let atanScale = fromIntegral to / maxAngleTangent
  let maxAngle = atan maxAngleTangent
  let maxHeight = 0.45 * fromIntegral winHeight
  let rows = rowCount room
  let cols = colCount room
  let maxD = (0.8 :: Double) * (fromIntegral $ rows + cols)
  flip mapM_ [from..to] $ \angle -> do
    let alpha = _heroDir s - (atan $ fromIntegral angle / atanScale)
    let d = case lineOfFlight (_heroX s) (_heroY s) alpha room of
              Horizon -> maxD
              Obstacle di _ _ _ -> di
    when (d < maxD) $ do
      let len = round $ maxHeight / (1 + d * (maxHeight - 1)/maxD)
      let rgb = round $ 176 * (maxD - d) / maxD
      let x = angle - from
      SDL.rendererDrawColor renderer SDL.$= SDL.V4 rgb rgb rgb 1
      SDL.drawLine renderer (SDL.P $ SDL.V2 x (horizont - len)) (SDL.P $ SDL.V2 x (horizont + len))
  SDL.rendererDrawColor renderer SDL.$= black
  let mapScale = 6 :: Position
  let mapScaleD = fromIntegral mapScale
  let toMapD x = round $ ((2 * x + 1) * mapScaleD) / 2
  flip mapM_ [0..rows - 1] $ \row -> do
    let rowRow = rows - row
    SDL.drawLine renderer (SDL.P $ fmap (toMapD . fromIntegral) $ SDL.V2 0 $ rowRow) $ SDL.P $ fmap (toMapD . fromIntegral) $ SDL.V2 (cols - 1) rowRow
    flip mapM_ [0..cols - 1] $ \col -> do
      when (row == 0) $
        SDL.drawLine renderer (SDL.P $ fmap (toMapD . fromIntegral) $ SDL.V2 col 1)
                            $ SDL.P $ fmap (toMapD . fromIntegral) $ SDL.V2 col $ rows
      if ((room `r12d` row) col == 0)
      then return ()
      else SDL.fillRect renderer $ Just $ SDL.Rectangle (SDL.P $ fmap (toMapD . (+ (negate 0.5)) . fromIntegral) $ SDL.V2 col $ rows - row)
                                                      $ SDL.V2 mapScale mapScale
  let megaBp offs = SDL.P $ fmap toMapD $ SDL.V2 (_heroX s + 15 * (cos $ _heroDir s + offs))
                                                 (fromIntegral rows - _heroY s - 15 * (sin $ _heroDir s + offs))
  let bp offs = SDL.P $ fmap toMapD $ SDL.V2 (_heroX s + (cos $ _heroDir s + offs))
                                             (fromIntegral rows - _heroY s - (sin $ _heroDir s + offs))
  let center = SDL.P $ fmap toMapD $ SDL.V2 (_heroX s) (fromIntegral rows - _heroY s)
  SDL.drawLine renderer center $ bp $ pi + 0.5
  SDL.drawLine renderer center $ bp $ pi - 0.5
  SDL.drawLine renderer center $ megaBp $ maxAngle
  SDL.drawLine renderer center $ megaBp $ negate maxAngle
  let fpsEst = _fpsEst s
  let fps = (take 5 $ show fpsEst) ++ " x=" ++ (take 5 $ show $ _heroX s) ++ " y=" ++ (take 5 $ show $ _heroY s)
  when (fpsEst > 5) $ do
    withStringTexture renderer font black fps $ \fpsTexture -> do
      SDL.TextureInfo { SDL.textureWidth = textWidth
                      , SDL.textureHeight = textHeight
                      } <- SDL.queryTexture fpsTexture
      SDL.copy renderer
               fpsTexture
               Nothing -- use complete fpsTexture as source
             $ Just $ SDL.Rectangle (SDL.P $ SDL.V2 0 $ winHeight - textHeight)
                                  $ SDL.V2 textWidth textHeight
  SDL.present renderer


main :: IO ()
main = withSDL $ do
  let width = 25
  let height = 25
  let aa = rowFirst2DArray height width [(r, c, if ((r <= 3 && c == 8)
                                                    || (r == height - 1 && (c < 7 || c > 12))
                                                    || c == 0
                                                    || (r == 2 && c `mod` 3 == 2)
                                                    || (r == 7 && c == 4)
                                                    || ((abs $ r + 2 * c - 18) <= 1 && r > 2 && r < 8)
                                                    || ((abs $ r + 2 * c - 26) <= 1 && r > 2 && r < 15)
                                                    || ((abs $ r - 17) <= 2 && (abs $ c - 11) == 2)
                                                    || (r > 2 && c == width - 1))
                                                then 1 else 0)
                                        | r <- [0..height - 1], c <- [0..width - 1]]
  let simple = rowFirst2DArray height width [(r, c, if ((r == 0 && c == 0)
                                                        || (r == height - 1 && c == 0)
                                                        || ((abs $ r - height `div` 2) == 2 && c == width - 4))
                                                    then 1 else 0)
                                            | r <- [0..height - 1], c <- [0..width - 1]]
  getArgs >>= \case
    [] -> putStrLn "Need a font...\n\
                   \cabal new-run wolfenhain \"$(fc-match --format \"%{file}\")\""
    (fontPath:x) ->
      withSDLFont fontPath 36 $ \font ->
        withWindow "Wolfenhain"
                   (fromIntegral winWidth, fromIntegral winHeight)
                 $ win (if null x then aa else simple) font

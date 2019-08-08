{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- This would have been impossible without
-- https://github.com/palf/haskell-sdl2-examples
--
-- Modifying the different lessons and mashing them together

module Main (main) where

import qualified SDL
import qualified SDL.Font
import SDL.Image

import Control.Exception      (handle, throw)
import Control.Monad          (void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Monoid            (First(..))
import Data.Text              (Text, pack)
import Data.Word              (Word8)
import System.Environment     (getArgs)

import Physics


withSDL :: (MonadIO m) => m a -> m ()
withSDL op = do
  SDL.initialize []
  void op
  SDL.quit


withSDLFont :: (MonadIO m) => FilePath -> SDL.Font.PointSize -> (SDL.Font.Font -> m a) -> m a
withSDLFont path fontSize op = do
  SDL.Font.initialize
  font <- SDL.Font.load path fontSize
  r <- op font
  SDL.Font.free font
  SDL.Font.quit
  return r


withWindow :: (MonadIO m) => Text -> (Int, Int) -> (SDL.Window -> m a) -> m ()
withWindow title (x, y) op = do
  w <- SDL.createWindow title p
  SDL.showWindow w
  void $ op w
  SDL.destroyWindow w

    where
      p = SDL.defaultWindow { SDL.windowInitialSize = z }
      z = SDL.V2 (fromIntegral x) (fromIntegral y)


renderSurfaceToWindow :: (MonadIO m) => SDL.Window -> SDL.Surface -> SDL.Surface -> m ()
renderSurfaceToWindow w s i
  = SDL.surfaceBlit i Nothing s Nothing
  >> SDL.updateWindowSurface w


isContinue :: Maybe SDL.Event -> Bool
isContinue = maybe True (not . isQuitEvent)


isQuitEvent :: SDL.Event -> Bool
isQuitEvent (SDL.Event _t SDL.QuitEvent) = True
isQuitEvent x = eventIsPress SDL.KeycodeSpace x


conditionallyRun :: Applicative m => m () -> Bool -> m Bool
conditionallyRun f True = True <$ f
conditionallyRun _ False = pure False


whileM :: Monad m => m Bool -> m ()
whileM a = do
  cont <- a
  case cont of
    True -> whileM a
    False -> return ()


win1 :: MonadIO m => SDL.Window -> m ()
win1 w = do
    screen <- SDL.getWindowSurface w
    image <- SDL.Image.load "./assets/cat100x100.png"
    let doRender = renderSurfaceToWindow w screen image
    whileM $
      isContinue <$> SDL.pollEvent
      >>= conditionallyRun doRender
    -- SDL.delay 5000 -- ms
    SDL.freeSurface image
    SDL.freeSurface screen


textTexture :: MonadIO m => SDL.Renderer -> SDL.Font.Font -> ColorPlusAlpha -> Text -> m SDL.Texture
textTexture renderer font color text = do
  textSurface <- SDL.Font.solid font color text
  result <- SDL.createTextureFromSurface renderer textSurface
  SDL.freeSurface textSurface
  return result


win2 :: MonadIO m => SDL.Font.Font -> SDL.Window -> m ()
win2 font w = do
    renderer <- liftIO $ handle ((\e -> do
                                    backupRendererIndex <- getSoftwareRendererIndex
                                    case backupRendererIndex of
                                      Just i -> putStrLn (show e ++ ", retrying with " ++ show i) >> mkRenderer i
                                      Nothing -> putStrLn "Can't find backup driver" >> throw e
                                 ) :: SDL.SDLException -> IO SDL.Renderer)
                              $ mkRenderer (-1)
    heroTexture <- SDL.Image.loadTexture renderer "./assets/sheet_hero_walk.png"
    catTexture <- SDL.Image.loadTexture renderer "./assets/cat100x100.png"
    startTicks <- SDL.ticks
    appLoop (AppState False startTicks 0 (IdleHero startTicks 2) startTicks 0.0 font heroTexture catTexture)
            renderer
    SDL.destroyTexture catTexture
    SDL.destroyTexture heroTexture
  where mkRenderer c = SDL.createRenderer w c SDL.defaultRenderer


eventIsPress :: SDL.Keycode -> SDL.Event -> Bool
eventIsPress keycode event =
  case SDL.eventPayload event of
    SDL.KeyboardEvent keyboardEvent ->
      SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
      SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == keycode
    _ -> False


data Hero =
  IdleHero GameTime Position
  | RunningHero MUA


data AppState = AppState {
  _isRed :: Bool
  , _sceneLastMove :: GameTime
  , _sceneOrigin :: Position
  , _heroState :: Hero
  , _lastTicks :: GameTime
  , _fpsEst :: Double
  , _font :: SDL.Font.Font
  , _heroTexture :: SDL.Texture
  , _catTexture :: SDL.Texture
  }


type ColorPlusAlpha = SDL.V4 Word8


black, blue, green, red :: ColorPlusAlpha
black = SDL.V4 0 0 0 0
blue = SDL.V4 0 0 255 0
green = SDL.V4 0 255 0 0
red = SDL.V4 255 0 0 0


winHeight, winWidth :: Position
winHeight = 480
winWidth = 640

maxScreenPos, minScreenPos :: Position
maxScreenPos = 2 * winWidth `div` 3
minScreenPos = winWidth `div` 8


updateAppTime :: GameTime -> AppState -> AppState
updateAppTime now s0@(AppState { _lastTicks=t0, _sceneLastMove=sceneLastMove, _sceneOrigin=sceneOrigin, _fpsEst=fps0, _heroState=hero })
  | now > t0 = s0 { _lastTicks=now
                  , _sceneLastMove=newSceneLastMove
                  , _sceneOrigin=newSceneOrigin
                  , _fpsEst=(pastWeight * fps0 + timeScaling / (fromIntegral $ now - t0))
                              / (pastWeight + 1)
                  , _heroState=case hero of
                                 IdleHero _ _ -> hero
                                 RunningHero mua
                                   | muaSpeed mua now > speedZero -> hero
                                   | otherwise -> IdleHero (muaTimeOfMaxDistance mua)
                                                         $ muaX0 mua + muaMaxDistance mua
                  }
  | otherwise = s0
  where pastWeight = 9 -- higher values mean more weight of the past FPS estimates in current estimate
        (newSceneLastMove, newSceneOrigin) =
          let heroPos = case hero of
                          IdleHero _ x0 -> x0
                          RunningHero mua -> muaX0 mua + muaDistance mua now in
            case (sceneLastMove + (round $ timeScaling / 4) < now -- update regularly
                 , heroPos > sceneOrigin + maxScreenPos -- don't let hero go too far to the right
                 , heroPos > sceneOrigin + minScreenPos -- but don't scroll when more or less in middle
                 ) of
              (_, True, _) -> (now, heroPos - maxScreenPos)
              (True, False, True) -> (now, sceneOrigin + 1)
              _ -> (sceneLastMove, sceneOrigin)


updateAppForEvent :: SDL.Event -> AppState -> Maybe AppState
updateAppForEvent (SDL.Event _t SDL.QuitEvent) _ = Nothing
updateAppForEvent e@(SDL.Event now _) s0
  | eventIsPress SDL.KeycodeQ e = Nothing
  | eventIsPress SDL.KeycodeB e = Just $ s0 { _isRed = not $ _isRed s0 }
  | eventIsPress SDL.KeycodeR e = Just $ s0 { _heroState =
      case _heroState s0 of
        IdleHero _ x0 -> RunningHero $ MUA heroAccel now heroSpeed x0
        RunningHero mua -> let (GS currentSpeed) = muaSpeed mua now
                               (GS initSpeed) = heroSpeed in
                           RunningHero $ MUA heroAccel
                                             now
                                             (GS $ initSpeed + (currentSpeed / 2))
                                           $ muaX0 mua + muaDistance mua now
    }
  | otherwise = Just s0
  where heroAccel = GA (-3.0)
        heroSpeed = GS (15.0)


appLoop :: MonadIO m => AppState -> SDL.Renderer -> m ()
appLoop oldState renderer = do
  events <- SDL.pollEvents
  case foldr (\e mbS -> mbS >>= updateAppForEvent e) (Just oldState) events of
    Nothing -> return ()
    Just s -> do
                now <- SDL.ticks
                let nextState = updateAppTime now s
                drawApp now nextState renderer
                appLoop nextState renderer


heroDrawInfo :: GameTime -> Hero -> (Int, Position, Position)
heroDrawInfo now (IdleHero t0 x0) =
  let timeDiff = now - t0
      jitter = max 0 $ (timeDiff `div` (round $ timeScaling / 2)) `mod` 3 - 1 in
    (fromIntegral $ (timeDiff `div` (round $ timeScaling / 3)) `mod` 2
    , x0 - 32
    , winHeight `div` 2 + fromIntegral jitter)
heroDrawInfo now (RunningHero mua) =
  let timeDiff = now - muaT0 mua
      frameCount = 3
      distance = muaDistance mua now
      GS speed = muaSpeed mua now in
    (-- switch from speed based animation to time based to maintain illusion
     -- of movement at low speeds
     if speed > 5
     then fromIntegral distance `mod` frameCount
     else (fromIntegral timeDiff `div` (round $ timeScaling / 5)) `mod` frameCount
    , muaX0 mua + distance - 32
    , winHeight `div` 2)


drawApp :: MonadIO m => GameTime -> AppState -> SDL.Renderer -> m ()
drawApp now (AppState isRed _ sceneOrigin heroState _ fpsEst font heroTexture catTexture) renderer = do
  SDL.rendererDrawColor renderer SDL.$= (if isRed then red else green)
  SDL.clear renderer
  SDL.rendererDrawColor renderer SDL.$= black
  SDL.drawLine renderer (SDL.P $ SDL.V2 minScreenPos 0) (SDL.P $ SDL.V2 minScreenPos winHeight)
  SDL.drawLine renderer (SDL.P $ SDL.V2 maxScreenPos 0) (SDL.P $ SDL.V2 maxScreenPos winHeight)
  let fps = (take 7 $ show fpsEst)
         ++ case heroState of
              IdleHero _ _ -> ""
              RunningHero mua -> " " ++ (take 7 . show $ muaSpeed mua now)
  liftIO $ putStrLn $ show now ++ ": FPS = " ++ fps
  case heroState of
    IdleHero _ _ -> return ()
    RunningHero mua ->
      liftIO $ putStrLn $ "  mua = " ++ (show $ muaDistance mua now) ++ " " ++ (show $ muaMaxDistance mua)
  when (fpsEst > 25) $ do
    fpsTexture <- textTexture renderer font (if isRed then blue else black) $ pack fps
    SDL.TextureInfo { SDL.textureWidth = textWidth
                    , SDL.textureHeight = textHeight
                    } <- SDL.queryTexture fpsTexture
    SDL.copy renderer
             fpsTexture
             Nothing -- use complete fpsTexture as source
           $ Just $ SDL.Rectangle (SDL.P $ SDL.V2 (winWidth - textWidth) (winHeight - textHeight))
                                $ SDL.V2 textWidth textHeight
    SDL.destroyTexture fpsTexture
  let catWidth = 100
  let catXs = take 2 $ filter (\x -> (x + catWidth > sceneOrigin))
                              [50, winWidth * 10 `div` 9 ..]
  flip mapM_ catXs $ \catX -> do
    SDL.copy renderer
             catTexture
             Nothing
           $ Just $ SDL.Rectangle (SDL.P $ SDL.V2 (catX - sceneOrigin) $ winHeight `div` 4)
                                $ SDL.V2 catWidth 100
  let (frame, x, y) = heroDrawInfo now heroState
  SDL.copy renderer
           heroTexture
           (Just $ SDL.Rectangle (SDL.P $ SDL.V2 (fromIntegral frame * 64) 0) $ SDL.V2 64 64)
         $ Just $ SDL.Rectangle (SDL.P $ SDL.V2 (x - sceneOrigin) y) $ SDL.V2 128 128
  SDL.present renderer


getSoftwareRendererIndex :: (Num a, Enum a) => MonadIO m => m (Maybe a)
getSoftwareRendererIndex = do
    drivers <- SDL.getRenderDriverInfo
    return $ getFirst $ foldMap indexIfIsSoftwareRenderer $ zip [0..] drivers
  where indexIfIsSoftwareRenderer (idx
                                  , SDL.RendererInfo {
                                      SDL.rendererInfoFlags = SDL.RendererConfig {
                                        SDL.rendererType = SDL.SoftwareRenderer}}
                                  ) = First $ Just idx
        indexIfIsSoftwareRenderer _ = First $ Nothing


main :: IO ()
main = do
  withSDL $ do
    -- Space or quit or close window to progress...
    withWindow "Lesson 01" (fromIntegral winWidth, fromIntegral winHeight) win1
    -- (q)uit, `b' to toggle color and `r' to make hero move
    getArgs >>= \case
      [] -> putStrLn "Second demo only with a font...\n\
                     \cabal new-run first-sdl2 \"$(fc-match --format \"%{file}\")\""
      [fontPath] ->
        withSDLFont fontPath 72 $ \font ->
          withWindow "http://hackage.haskell.org/package/sdl2-2.5.0.0/docs/SDL.html"
                     (fromIntegral winWidth, fromIntegral winHeight)
                   $ win2 font
      _ -> putStrLn "Only one font allowed"

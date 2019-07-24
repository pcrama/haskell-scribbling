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
import Control.Monad          (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Monoid            (First(..))
import Data.Text              (Text, pack)
import Data.Word              (Word32, Word8)
import Foreign.C.Types        (CInt)
import System.Environment     (getArgs)


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
    fpsTexture <- textTexture renderer font black "???"
    startTicks <- SDL.ticks
    appLoop (AppState False startTicks 0 font fpsTexture) renderer
  where mkRenderer c = SDL.createRenderer w c SDL.defaultRenderer


eventIsPress :: SDL.Keycode -> SDL.Event -> Bool
eventIsPress keycode event =
  case SDL.eventPayload event of
    SDL.KeyboardEvent keyboardEvent ->
      SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
      SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == keycode
    _ -> False


data AppState = AppState {
  _isRed :: Bool
  , _lastTicks :: Word32
  , _frameCount :: Int
  , _font :: SDL.Font.Font
  , _fps :: SDL.Texture
  }


type ColorPlusAlpha = SDL.V4 Word8


black, blue, green, red :: ColorPlusAlpha
black = SDL.V4 0 0 0 0
blue = SDL.V4 0 0 255 0
green = SDL.V4 0 255 0 0
red = SDL.V4 255 0 0 0


winHeight, winWidth :: CInt
winHeight = 480
winWidth = 640


appLoop :: MonadIO m => AppState -> SDL.Renderer -> m ()
appLoop oldState@(AppState isRed lastTicks frames font fpsTexture) renderer = do
  events <- SDL.pollEvents
  now <- SDL.ticks
  let qPressed = any (eventIsPress SDL.KeycodeQ) events
      rPressed = any (eventIsPress SDL.KeycodeR) events
  let timeDiff = now - lastTicks
  let atLeastOneSecond = timeDiff >= 1000
  let n = (winHeight * fromIntegral timeDiff) `div` 1000
  SDL.rendererDrawColor renderer SDL.$= (if isRed then red else green)
  SDL.clear renderer
  SDL.rendererDrawColor renderer SDL.$= (if isRed then green else blue)
  SDL.fillRect renderer (Just $ SDL.Rectangle (SDL.P $ SDL.V2 0 0) (SDL.V2 n n))
  fpsTexture' <- case atLeastOneSecond of
    True -> do
      let fps = show (fromIntegral frames * (1000.0 :: Double) / fromIntegral timeDiff)
      liftIO $ putStrLn $ show now ++ ": FPS = " ++ fps
      SDL.destroyTexture fpsTexture
      r <- textTexture renderer font black $ pack $ take 7 fps
      return r
    False ->
      return fpsTexture
  SDL.TextureInfo { SDL.textureWidth = textWidth
                  , SDL.textureHeight = textHeight
                  } <- SDL.queryTexture fpsTexture'
  let revT = max 0 $ 1000 - fromIntegral timeDiff -- from 1000 downto 0
  SDL.copy renderer
           fpsTexture'
           Nothing -- use complete fpsTexture' as source
         $ Just $ SDL.Rectangle (SDL.P $ SDL.V2 ((winWidth - textWidth) * revT `div` 1000)
                                              $ (winHeight - textHeight)
                                                * (revT - 500) * (revT - 500)
                                                `div` (2 * 500 * 500))
                              $ SDL.V2 textWidth textHeight
  SDL.present renderer
  let (newFrameCount, newLastTicks) = if atLeastOneSecond
                                      then (0, now)
                                      else (frames + 1, lastTicks)
  if qPressed
  then SDL.destroyTexture fpsTexture'
  else appLoop oldState { _isRed = if rPressed || atLeastOneSecond then not isRed else isRed
                        , _lastTicks = newLastTicks
                        , _frameCount = newFrameCount
                        , _fps = fpsTexture'
                        }
               renderer


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
    -- (q)uit and `r' to toggle color
    getArgs >>= \case
      [] -> putStrLn "Second demo only with a font...\
                    \ncabal new-run first-sdl2 \"$(fc-match --format \"%{file}\" | head -n 1)\""
      [fontPath] ->
        withSDLFont fontPath 72 $ \font ->
          withWindow "http://hackage.haskell.org/package/sdl2-2.5.0.0/docs/SDL.html"
                     (fromIntegral winWidth, fromIntegral winHeight)
                   $ win2 font
      _ -> putStrLn "Only one font allowed"

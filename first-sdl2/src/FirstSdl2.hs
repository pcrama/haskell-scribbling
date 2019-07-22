{-# LANGUAGE OverloadedStrings #-}

-- This would have been impossible without
-- https://github.com/palf/haskell-sdl2-examples
--
-- Modifying the different lessons and mashing them together

module Main (main) where

import qualified SDL
import SDL.Image

import Control.Exception      (handle, throw)
import Control.Monad          (unless, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Monoid            (First(..))
import Data.Text              (Text)


withSDL :: (MonadIO m) => m a -> m ()
withSDL op = do
  SDL.initialize []
  void op
  SDL.quit


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

    -- SDL.surfaceFillRect screen Nothing (SDL.V4 maxBound minBound maxBound maxBound)
    -- SDL.updateWindowSurface w

    let doRender = renderSurfaceToWindow w screen image

    whileM $
      isContinue <$> SDL.pollEvent
      >>= conditionallyRun doRender
    -- SDL.delay 5000 -- ms
    SDL.freeSurface image
    SDL.freeSurface screen


win2 :: MonadIO m => SDL.Window -> m ()
win2 w = do
    renderer <- liftIO $ handle ((\e -> do
                                    backupRendererIndex <- getSoftwareRendererIndex
                                    case backupRendererIndex of
                                      Just i -> putStrLn (show e ++ ", retrying with " ++ show i) >> mkRenderer i
                                      Nothing -> putStrLn "Can't find backup driver" >> throw e
                                 ) :: SDL.SDLException -> IO SDL.Renderer)
                              $ mkRenderer (-1)
    appLoop False renderer
  where mkRenderer c = SDL.createRenderer w c SDL.defaultRenderer


eventIsPress :: SDL.Keycode -> SDL.Event -> Bool
eventIsPress keycode event =
  case SDL.eventPayload event of
    SDL.KeyboardEvent keyboardEvent ->
      SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
      SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == keycode
    _ -> False


appLoop :: MonadIO m => Bool -> SDL.Renderer -> m ()
appLoop isRed renderer = do
  events <- SDL.pollEvents
  let qPressed = any (eventIsPress SDL.KeycodeQ) events
      rPressed = any (eventIsPress SDL.KeycodeR) events
  SDL.rendererDrawColor renderer SDL.$= (if isRed then (SDL.V4 0 0 255 255) else (SDL.V4 0 255 0 255))
  SDL.clear renderer
  SDL.present renderer
  unless qPressed $ appLoop (rPressed /= isRed) renderer


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
    withWindow "Lesson 01" (640, 480) win1
    -- (q)uit and `r' to toggle color
    withWindow "http://hackage.haskell.org/package/sdl2-2.5.0.0/docs/SDL.html" (640, 480) win2

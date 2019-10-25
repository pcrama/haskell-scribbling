module AllSDL (
  ColorPlusAlpha
  , black
  , blue
  , eventIsChar
  , eventIsPress
  , getSoftwareRendererIndex
  , green
  , lSDLcopy
  , red
  , winHeight
  , winWidth
  , withImageTextures
  , withNonEmptyTexture
  , withSDL
  , withSDLFont
  , withStringTexture
  , withWindow
) where

import qualified SDL
import qualified SDL.Font
import SDL.Image

import Control.Monad          (void)
import Control.Monad.IO.Class (MonadIO)
import Data.List.NonEmpty     (NonEmpty(..))
import Data.Monoid            (First(..))
import Data.Text              (Text, pack)
import Data.Word              (Word8)

import Physics


winHeight, winWidth :: Position
winHeight = 480
winWidth = 640


type ColorPlusAlpha = SDL.V4 Word8


black, blue, green, red :: ColorPlusAlpha
black = SDL.V4 0 0 0 0
blue = SDL.V4 0 0 255 0
green = SDL.V4 0 255 0 0
red = SDL.V4 255 0 0 0


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


textTexture :: MonadIO m => SDL.Renderer -> SDL.Font.Font -> ColorPlusAlpha -> Text -> m SDL.Texture
textTexture renderer font color text = do
  textSurface <- SDL.Font.solid font color text
  result <- SDL.createTextureFromSurface renderer textSurface
  SDL.freeSurface textSurface
  return result


withStringTexture :: MonadIO m
                  => SDL.Renderer
                  -> SDL.Font.Font
                  -> ColorPlusAlpha
                  -> String
                  -> (SDL.Texture -> m a)
                  -> m a
withStringTexture renderer font color s f = do
  texture <- textTexture renderer font color $ pack s
  result <- f texture
  SDL.destroyTexture texture
  return result


withNonEmptyTexture :: MonadIO m
                    => SDL.Renderer
                    -> SDL.Font.Font
                    -> ColorPlusAlpha
                    -> NonEmpty Char
                    -> (SDL.Texture -> m a)
                    -> m a
withNonEmptyTexture renderer font color (x:|xs) f =
  withStringTexture renderer font color (x:xs) f


-- | Execute action with images loaded from files & clean up
withImageTextures :: MonadIO m
                  => SDL.Renderer -- ^ SDL renderer
                  -> [FilePath] -- ^ List of images to load
                  -> ([SDL.Texture] -> m a) -- ^ Action to execute, param count must match image count
                  -> m a -- ^ result of action
withImageTextures renderer = go []
  where go acc [] f = f $ reverse acc
        go acc (x:xs) f = withTexture x (\t -> go (t:acc) xs f)
        withTexture path action = do
          texture <- SDL.Image.loadTexture renderer path
          result <- action texture
          SDL.destroyTexture texture
          return result


-- | Wrapper around SDL.copy, adding bounding box
lSDLcopy :: MonadIO m
         => SDL.Renderer
         -> SDL.Texture
         -> Maybe (SDL.Rectangle Position)
         -> Maybe (SDL.Rectangle Position)
         -> m ()
lSDLcopy renderer texture src dest = do
  case dest of
    Just (SDL.Rectangle (SDL.P (SDL.V2 x0 y0)) (SDL.V2 w h)) -> do
      let x1 = x0 + w
      let y1 = y0 + h
      SDL.drawLine renderer (SDL.P $ SDL.V2 x0 y0) (SDL.P $ SDL.V2 x1 y0)
      SDL.drawLine renderer (SDL.P $ SDL.V2 x0 y0) (SDL.P $ SDL.V2 x0 y1)
      SDL.drawLine renderer (SDL.P $ SDL.V2 x1 y1) (SDL.P $ SDL.V2 x1 y0)
      SDL.drawLine renderer (SDL.P $ SDL.V2 x1 y1) (SDL.P $ SDL.V2 x0 y1)
    Nothing -> return ()
  SDL.copy renderer texture src dest


eventIsPress :: SDL.Keycode -> SDL.Event -> Bool
eventIsPress keycode event =
  case SDL.eventPayload event of
    SDL.KeyboardEvent keyboardEvent ->
      SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
      SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == keycode
    _ -> False


eventIsChar :: (SDL.Keycode -> Maybe Char) -> SDL.Event -> Char -> Bool
eventIsChar keymap event c =
  case SDL.eventPayload event of
    SDL.KeyboardEvent keyboardEvent ->
      SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed
      && (keymap $ SDL.keysymKeycode $ SDL.keyboardEventKeysym keyboardEvent) == Just c
    _ -> False


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

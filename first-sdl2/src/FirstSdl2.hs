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

import Control.Concurrent     (threadDelay)
import Control.Exception      (handle, throw)
import Control.Monad          (void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Monoid            (First(..))
import Data.List.NonEmpty     (NonEmpty(..))
import Data.Text              (Text, pack)
import Data.Word              (Word8)
import System.Environment     (getArgs)

import Physics
import Keymaps


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


withImageTexture :: MonadIO m => SDL.Renderer -> FilePath -> (SDL.Texture -> m a) -> m a
withImageTexture renderer path action = do
  texture <- SDL.Image.loadTexture renderer path
  result <- action texture
  SDL.destroyTexture texture
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
    withImageTexture renderer "./assets/sheet_hero_walk.png" $ \heroTexture ->
      withImageTexture renderer "./assets/sheet_hero_idle.png" $ \heroIdleTexture ->
        withImageTexture renderer "./assets/cat100x100.png" $ \catTexture -> do
          startTicks <- SDL.ticks
          appLoop (AppState False startTicks 0 (IdleHero startTicks 2) startTicks 0.0 (Waiting ('f':|"osse") ('j':|"oie")) [(100, 's':|"osie", 'u':|"tile"), (200, 'g':|"out", 'l':|"oup")])
                $ AppContext renderer font heroTexture heroIdleTexture catTexture azerty_on_qwerty
  where mkRenderer c = SDL.createRenderer w c SDL.defaultRenderer


eventIsPress :: SDL.Keycode -> SDL.Event -> Bool
eventIsPress keycode event =
  case SDL.eventPayload event of
    SDL.KeyboardEvent keyboardEvent ->
      SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
      SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == keycode
    _ -> False


eventIsChar :: (SDL.Keycode -> Maybe Char) -> Char -> SDL.Event -> Bool
eventIsChar keymap c event =
  case SDL.eventPayload event of
    SDL.KeyboardEvent keyboardEvent ->
      SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed
      && (keymap $ SDL.keysymKeycode $ SDL.keyboardEventKeysym keyboardEvent) == Just c
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
  , _typing :: TypingState
  , _words :: [(Position, NonEmpty Char, NonEmpty Char)]
  }


data AppContext = AppContext {
  _renderer :: SDL.Renderer
  , _font :: SDL.Font.Font
  , _heroTexture :: SDL.Texture
  , _heroIdleTexture :: SDL.Texture
  , _catTexture :: SDL.Texture
  , _keymap :: SDL.Keycode -> Maybe Char
  }

-- | States for typing exercise
--
-- - Waiting: no recognizable key has been pressed yet.  Two words may be
--   typed, one to initiate running and one to initiate jumping.  As soon
--   as the hero gets far enough for a new word, go to the Transition state.
--   As soon the first key of either is pressed, go to the Typing state.
--
-- - Transition: this presents the player with new words to run or jump, but
--   allows the player to still start with the older words.  As soon as a
--   key matching any of the 4 first characters is pressed, go to the Typing
--   state.
--
-- - Typing: a part of a word has been entered, remember how much was typed
--   and how much remains.  This information is updated each time a key
--   matching the first character is typed.  As soon as the last character
--   is seen, go back to the Waiting state.  The state also carries the
--   words to use when going back to the Waiting state.
data TypingState =
  Waiting (NonEmpty Char) (NonEmpty Char) -- ^ type to run, type to jump
  | Transition GameTime (NonEmpty Char) (NonEmpty Char) (NonEmpty Char) (NonEmpty Char) -- ^ when to switch, old run, old jump, new run, new jump
  | Typing (NonEmpty Char) (NonEmpty Char) (NonEmpty Char) (NonEmpty Char) -- ^ already typed, still to type, type to run to fall back to Waiting, type to jump to fall back to Waiting


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
updateAppTime now s0@(AppState { _lastTicks=t0, _sceneLastMove=sceneLastMove, _sceneOrigin=sceneOrigin, _fpsEst=fps0, _heroState=hero, _typing=typing, _words=words })
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
                  , _typing=typing'
                  , _words=words'
                  }
  | otherwise = s0
  where pastWeight = 9 -- higher values mean more weight of the past FPS estimates in current estimate
        heroPos = case hero of
                    IdleHero _ x0 -> x0
                    RunningHero mua -> muaX0 mua + muaDistance mua now
        (newSceneLastMove, newSceneOrigin) =
          case (sceneLastMove + (round $ timeScaling / 4) < now -- update regularly
               , heroPos > sceneOrigin + maxScreenPos -- don't let hero go too far to the right
               , heroPos > sceneOrigin + minScreenPos -- but don't scroll when more or less in middle
               ) of
            (_, True, _) -> (now, heroPos - maxScreenPos)
            (True, False, True) -> (now, sceneOrigin + 1)
            _ -> (sceneLastMove, sceneOrigin)
        (typing', words') =
          let noChange = (typing, words)
              mbNewWords = case words of
                             (x, newRun, newJump):nextWords -> Just (x, newRun, newJump, nextWords)
                             [] -> Nothing in
          case (typing, mbNewWords) of
            (Waiting _ _, Nothing) -> noChange
            (Waiting oldRun oldJump, Just (x, newRun, newJump, nextWords))
              | heroPos > x -> (Transition (now + (round $ timeScaling / 2))
                                           oldRun oldJump newRun newJump
                               , nextWords)
              | otherwise -> noChange
            (Transition t _ _ newRun newJump, _)
              | now > t -> (Waiting newRun newJump, words)
              | otherwise -> noChange
            (Typing _ _ _ _, _) -> noChange


updateAppForEvent :: SDL.Event -> AppContext -> AppState -> Maybe AppState
updateAppForEvent (SDL.Event _t SDL.QuitEvent) _ _ = Nothing
updateAppForEvent e@(SDL.Event now _) (AppContext { _keymap=keymap }) s0
  | eventIsPress SDL.KeycodeEscape e = Nothing
  | eventIsPress SDL.KeycodeSpace e = Just $ s0 { _isRed = not $ _isRed s0 }
  | eventIsPress SDL.KeycodeBackspace e =
                case _typing s0 of
                  Waiting _ _ -> noChange
                  Typing _ _ oldRun oldJump -> Just $ s0 { _typing=Waiting oldRun oldJump }
                  Transition _ _ _ _ _ -> noChange
  | otherwise = case _typing s0 of
                  Waiting run@(x:|xs) jump -> if eventIsChar keymap x e
                                              then startToType x xs run jump
                                              else noChange
                  Typing already
                         (x:|xs)
                         oldRun
                         oldJump -> if eventIsChar keymap x e
                                    then case xs of
                                           [] -> startToRun oldRun oldJump
                                           (y:ys) -> Just $ s0 { _typing=Typing (already <> (x:|[]))
                                                                                (y:|ys)
                                                                                oldRun
                                                                                oldJump
                                                               }
                                    else noChange
                  Transition _
                             (x:|xs)
                             _
                             newRun@(y:|ys)
                             newJump -> case (eventIsChar keymap x e
                                             , eventIsChar keymap y e) of
                                          (True, _) -> startToType x xs newRun newJump
                                          (_, True) -> startToType y ys newRun newJump
                                          (_, _) -> noChange
  where heroAccel = GA (-3.0)
        heroSpeed = GS (15.0)
        noChange = Just s0
        startToType x (n:xs) run jump = Just $ s0 { _typing=Typing (x:|[]) (n:|xs) run jump }
        startToType _ _ _ _ = Nothing -- aborts the game if a 1 letter word is used
        startToRun run jump = Just $ s0 {
          _typing=Waiting run jump
          , _heroState=case _heroState s0 of
              IdleHero _ x0 -> RunningHero $ MUA heroAccel now heroSpeed x0
              RunningHero mua -> let (GS currentSpeed) = muaSpeed mua now
                                     (GS initSpeed) = heroSpeed in
                                 RunningHero $ MUA heroAccel
                                                   now
                                                   (GS $ initSpeed + (currentSpeed / 2))
                                                 $ muaX0 mua + muaDistance mua now
          }


appLoop :: MonadIO m => AppState -> AppContext -> m ()
appLoop oldState context = do
  events <- SDL.pollEvents
  case foldr (\e mbS -> mbS >>= updateAppForEvent e context) (Just oldState) events of
    Nothing -> return ()
    Just s -> do
                now <- SDL.ticks
                let nextState = updateAppTime now s
                drawApp now nextState context
                when (_fpsEst nextState > 100) $
                  liftIO $ threadDelay $ 10 * 1000 -- microseconds
                appLoop nextState context


heroDrawInfo :: GameTime -> Hero -> AppContext -> (SDL.Texture, Int, Position, Position)
heroDrawInfo now (IdleHero t0 x0) context =
  let timeDiff = now - t0 in
    (_heroIdleTexture context
    , fromIntegral $ (timeDiff `div` (round $ timeScaling / 4)) `mod` 8
    , x0 - 32
    , winHeight `div` 2)
heroDrawInfo now (RunningHero mua) context =
  let timeDiff = now - muaT0 mua
      frameCount = 3
      distance = muaDistance mua now
      GS speed = muaSpeed mua now in
    (_heroTexture context
     -- switch from speed based animation to time based to maintain illusion
     -- of movement at low speeds
    , if speed > 5
      then fromIntegral distance `mod` frameCount
      else (fromIntegral timeDiff `div` (round $ timeScaling / 5)) `mod` frameCount
    , muaX0 mua + distance - 32
    , winHeight `div` 2)


drawApp :: MonadIO m => GameTime -> AppState -> AppContext -> m ()
drawApp now
        (AppState isRed _ sceneOrigin heroState _ fpsEst typing _)
        context@(AppContext { _renderer=renderer, _font=font, _catTexture=catTexture }) = do
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
      liftIO $ putStrLn $ "  mua = "
                       ++ (show $ muaDistance mua now)
                       ++ " " ++ (show $ muaMaxDistance mua)
                       ++ " x = " ++ (show $ muaDistance mua now + muaX0 mua)
                       ++ " o = " ++ (show $ sceneOrigin)
  when (fpsEst > 25) $ do
    withStringTexture renderer font (if isRed then blue else black) fps $ \fpsTexture -> do
      SDL.TextureInfo { SDL.textureWidth = textWidth
                      , SDL.textureHeight = textHeight
                      } <- SDL.queryTexture fpsTexture
      SDL.copy renderer
               fpsTexture
               Nothing -- use complete fpsTexture as source
             $ Just $ SDL.Rectangle (SDL.P $ SDL.V2 (winWidth - textWidth) (winHeight - textHeight))
                                  $ SDL.V2 textWidth textHeight
  let catWidth = 100
  let catXs = take 2 $ filter (\x -> (x + catWidth > sceneOrigin))
                              [50, winWidth * 10 `div` 9 ..]
  flip mapM_ catXs $ \catX -> do
    SDL.copy renderer
             catTexture
             Nothing
           $ Just $ SDL.Rectangle (SDL.P $ SDL.V2 (catX - sceneOrigin) $ winHeight `div` 4)
                                $ SDL.V2 catWidth 100
  case typing of
    Waiting toRun _ ->
      withNonEmptyTexture renderer font black toRun $ \text -> do
        SDL.TextureInfo { SDL.textureWidth = textWidth
                        , SDL.textureHeight = textHeight
                        } <- SDL.queryTexture text
        SDL.copy renderer
                 text
                 Nothing -- use complete texture as source
               $ Just $ SDL.Rectangle (SDL.P $ SDL.V2 (winWidth - textWidth) 0)
                                    $ SDL.V2 textWidth textHeight
    Transition _ _ _ toRun _ ->
      withNonEmptyTexture renderer font black toRun $ \text -> do
        SDL.TextureInfo { SDL.textureWidth = textWidth
                        , SDL.textureHeight = textHeight
                        } <- SDL.queryTexture text
        SDL.copy renderer
                 text
                 Nothing -- use complete texture as source
               $ Just $ SDL.Rectangle (SDL.P $ SDL.V2 (winWidth - textWidth) 0)
                                    $ SDL.V2 textWidth textHeight
    Typing done toType _ _ ->
      withNonEmptyTexture renderer font (if isRed then blue else black) done $ \doneTexture -> do
        withNonEmptyTexture renderer font (if isRed then black else blue) toType $ \toTypeTexture -> do
          SDL.TextureInfo { SDL.textureWidth = textWidth
                          , SDL.textureHeight = textHeight
                          } <- SDL.queryTexture toTypeTexture
          SDL.copy renderer
                   toTypeTexture
                   Nothing -- use complete texture as source
                 $ Just $ SDL.Rectangle (SDL.P $ SDL.V2 (winWidth - textWidth) 0)
                                      $ SDL.V2 textWidth textHeight
          SDL.TextureInfo { SDL.textureWidth = doneWidth
                          , SDL.textureHeight = _
                          } <- SDL.queryTexture doneTexture
          SDL.copy renderer
                   doneTexture
                   Nothing -- use complete texture as source
                 $ Just $ SDL.Rectangle (SDL.P $ SDL.V2 (winWidth - textWidth - doneWidth) 0)
                                      $ SDL.V2 doneWidth textHeight
  let (texture, frame, x, y) = heroDrawInfo now heroState context
  SDL.copy renderer
           texture
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

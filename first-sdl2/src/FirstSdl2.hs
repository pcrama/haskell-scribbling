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


win2 :: MonadIO m => SDL.Font.Font -> SDL.Window -> m ()
win2 font w = do
    renderer <- liftIO $ handle ((\e -> do
                                    backupRendererIndex <- getSoftwareRendererIndex
                                    case backupRendererIndex of
                                      Just i -> putStrLn (show e ++ ", retrying with " ++ show i) >> mkRenderer i
                                      Nothing -> putStrLn "Can't find backup driver" >> throw e
                                 ) :: SDL.SDLException -> IO SDL.Renderer)
                              $ mkRenderer (-1)
    withImageTextures renderer
                      ["./assets/sheet_hero_walk.png"
                      , "./assets/sheet_hero_idle.png"
                      , "./assets/sheet_hero_jump.png"
                      , "./assets/cat100x100.png"
                      , "./assets/sheet_snake_walk.png"
                      , "./assets/sheet_snake_hurt.png"]
                    $ \[heroTexture
                       , heroIdleTexture
                       , heroJumpTexture
                       , catTexture
                       , snakeTexture
                       , snakeDieTexture] -> do
            startTicks <- SDL.ticks
            appLoop (AppState False startTicks 0 (IdleHero startTicks minScreenPos) startTicks 0.0 (Waiting ('f':|"osse") ('j':|"o")) [(100, 's':|"osie", 'l':|"ollipops"), (200, 'g':|"out", 't':|"oi"), (300, 'b':|"oire", 'p':|"oisse")] (snakes startTicks))
                  $ AppContext renderer
                               font
                               heroTexture
                               heroIdleTexture
                               heroJumpTexture
                               catTexture
                               snakeTexture
                               snakeDieTexture
                               azerty_on_qwerty
  where mkRenderer c = SDL.createRenderer w c SDL.defaultRenderer
        snakes t0 = zipWith (\idx word -> MovingSnake (idx * winWidth + (winWidth * 3 `div` 4)) t0 word)
                            [0..]
                          $ cycle ['d':|"ur", 'd':|"oux", 'm':|"olle", 'r':|"ose"]


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


data Hero =
  IdleHero GameTime Position
  | RunningHero MUA
  | JumpingHero Jump


data Snake =
  MovingSnake Position GameTime (NonEmpty Char) -- initial position, `kill' word
  | DyingSnake Position GameTime -- static position, time at which snake should disappear


snakePosition :: Snake -> GameTime -> Position
snakePosition (MovingSnake x0 t0 _) now
  | now > t0 = x0 - (round $ 5 * (now `timeDiff` t0))
  | otherwise = x0
snakePosition (DyingSnake x _) _ = x


data AppState = AppState {
  _isRed :: Bool
  , _sceneLastMove :: GameTime
  , _sceneOrigin :: Position
  , _heroState :: Hero
  , _lastTicks :: GameTime
  , _fpsEst :: Double
  , _typing :: TypingState
  , _words :: [(Position, NonEmpty Char, NonEmpty Char)]
  , _snakes :: [Snake]
  }


data AppContext = AppContext {
  _renderer :: SDL.Renderer
  , _font :: SDL.Font.Font
  , _heroTexture :: SDL.Texture
  , _heroIdleTexture :: SDL.Texture
  , _heroJumpTexture :: SDL.Texture
  , _catTexture :: SDL.Texture
  , _snakeTexture :: SDL.Texture
  , _snakeDieTexture :: SDL.Texture
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
  | Typing (NonEmpty Char) (NonEmpty Char) (NonEmpty Char) (NonEmpty Char) (NonEmpty Char -> NonEmpty Char -> AppState -> Maybe AppState) -- ^ already typed, still to type, type to run to fall back to Waiting, type to jump to fall back to Waiting, continuation once the word is typed in full


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


heroPosition :: GameTime -> Hero -> Position
heroPosition _   (IdleHero _ x0)    = x0
heroPosition now (RunningHero mua)  = muaX0 mua + muaDistance mua now
heroPosition now (JumpingHero jump) = fst $ jumpPosition jump now


updateAppTime :: GameTime -> AppState -> AppState
updateAppTime now s0@(AppState { _lastTicks=t0, _sceneLastMove=sceneLastMove, _sceneOrigin=sceneOrigin, _fpsEst=fps0, _heroState=hero, _typing=typing, _words=wordList, _snakes=snakes })
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
                                 JumpingHero jump ->
                                   let maxJumpTime = jumpEndTime jump in
                                   if now > maxJumpTime
                                   then IdleHero maxJumpTime
                                               $ jumpX0 jump + jumpDistance jump maxJumpTime
                                   else hero
                  , _typing=typing'
                  , _words=words'
                  , _snakes=snakes'
                  }
  | otherwise = s0
  where pastWeight = 9 -- higher values mean more weight of the past FPS estimates in current estimate
        heroPos = heroPosition now hero
        (newSceneLastMove, newSceneOrigin) =
          case (sceneLastMove + (round $ timeScaling / 4) < now -- update regularly
               , heroPos > sceneOrigin + maxScreenPos -- don't let hero go too far to the right
               , heroPos > sceneOrigin + minScreenPos -- but don't scroll when more or less in middle
               ) of
            (_, True, _) -> (now, heroPos - maxScreenPos)
            (True, False, True) -> (now, sceneOrigin + 1)
            _ -> (sceneLastMove, sceneOrigin)
        (typing', words') =
          let noChange = (typing, wordList)
              mbNewWords = case wordList of
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
              | now > t -> (Waiting newRun newJump, wordList)
              | otherwise -> noChange
            (Typing _ _ _ _ _, _) -> noChange
        snakes' = removeSnakes snakes
        removeSnakes [] = []
        removeSnakes (s@(MovingSnake _ _ _):rest)
            | snakePos > heroPos + winWidth = s:rest -- no more filtering: snakes are outside of hero's view
            | snakePos < heroPos - winWidth = removeSnakes rest
            | otherwise = s:removeSnakes rest
          where snakePos = snakePosition s now
        removeSnakes (s@(DyingSnake _ timeout):rest)
            | now > timeout = removeSnakes rest
            | otherwise = s:removeSnakes rest


updateAppForEvent :: SDL.Event -> AppContext -> AppState -> Maybe AppState
updateAppForEvent (SDL.Event _t SDL.QuitEvent) _ _ = Nothing
updateAppForEvent e@(SDL.Event now _) (AppContext { _keymap=keymap }) s0
  | eventIsPress SDL.KeycodeEscape e = Nothing
  | eventIsPress SDL.KeycodeSpace e = Just $ s0 { _isRed = not $ _isRed s0 }
  | eventIsPress SDL.KeycodeBackspace e =
                case _typing s0 of
                  Waiting _ _ -> noChange
                  Typing _ _ oldRun oldJump _ -> Just $ s0 { _typing=Waiting oldRun oldJump }
                  Transition _ _ _ _ _ -> noChange
  | otherwise = case _typing s0 of
                  Waiting run@(x:|xs) jump@(j:|js) ->
                    case lookup True
                              $ (matchChar x
                                 , startToType x xs run jump startToRun
                              ):(matchChar j
                                 , startToType j js run jump startToJump
                              ):snakeKiller run jump of
                      Just r -> r
                      Nothing -> noChange
                  Typing already
                         (x:|xs)
                         oldRun
                         oldJump
                         cont -> if matchChar x
                                 then case xs of
                                        [] -> cont oldRun oldJump s0
                                        (y:ys) -> Just $ s0 { _typing=Typing (already <> (x:|[]))
                                                                             (y:|ys)
                                                                             oldRun
                                                                             oldJump
                                                                             cont
                                                            }
                                 else noChange
                  Transition _
                             (x:|xs)
                             (j:|js)
                             newRun@(y:|ys)
                             newJump@(z:|zs) ->
                               case lookup True
                                         $ (matchChar y
                                            , startToType y ys newRun newJump startToRun
                                         ):(matchChar z
                                            , startToType z zs newRun newJump startToJump
                                         ):(snakeKiller newRun newJump
                                            ++ [(matchChar x
                                                , startToType x xs newRun newJump startToRun)
                                               , (matchChar j
                                                 , startToType j js newRun newJump startToJump)]) of
                                 Just r -> r
                                 Nothing -> noChange
  where heroAccel = GA (-3.0)
        heroSpeed = GS (15.0)
        jumpHorizSpeed = GS (5.0)
        noChange = Just s0
        matchChar = eventIsChar keymap e
        snakeKiller r j = let heroPos = heroPosition now $ _heroState s0
                              snakes = _snakes s0
                              sceneOrigin = _sceneOrigin s0 in
                            map (\ (x0, s:|ss) -> (matchChar s
                                                  , startToType s ss r j $ startToKill x0))
                              $ killableSnakes now sceneOrigin heroPos snakes
        startToType x (n:xs) r j f = Just $ s0 { _typing=Typing (x:|[]) (n:|xs) r j f }
        startToType  _ _ _ _ _ = Nothing -- aborts the game if a 1 letter word is used
        killASnake _ [] = []
        killASnake x0 (s@(MovingSnake y0 _ _):tl)
          | x0 == y0 = (DyingSnake (snakePosition s now)
                                 $ now + (round $ 0.5 * timeScaling)):tl
          | x0 < y0 = tl -- avoid looping through infinite list of snakes
          | otherwise = s:killASnake x0 tl
        killASnake x0 (s@(DyingSnake _ _):tl) = s:killASnake x0 tl
        startToKill x0 run jump s@(AppState { _snakes=snakes0 }) = Just $ s {
          _typing=Waiting run jump
          , _snakes=killASnake x0 snakes0
          }
        startToRun run jump s = Just $ s {
          _typing=Waiting run jump
          , _heroState=case _heroState s of
              IdleHero _ x0 -> RunningHero $ MUA heroAccel now heroSpeed x0
              RunningHero mua -> let (GS currentSpeed) = muaSpeed mua now
                                     (GS initSpeed) = heroSpeed in
                                 RunningHero $ MUA heroAccel
                                                   now
                                                   (GS $ initSpeed + (currentSpeed / 2))
                                                 $ muaX0 mua + muaDistance mua now
              JumpingHero _ -> _heroState s -- you can't run if you're already jumping
          }
        startToJump runWord jumpWord s = Just $ s {
          _typing=Waiting runWord jumpWord
          , _heroState=case _heroState s of
              IdleHero _ x0 -> JumpingHero $ Jump x0
                                                  jumpHorizSpeed
                                                $ MUA heroAccel now heroSpeed 0
              RunningHero mua -> let (GS currentSpeed) = muaSpeed mua now
                                     (GS initSpeed) = jumpHorizSpeed in
                                 JumpingHero $ Jump (muaX0 mua + muaDistance mua now)
                                                    (GS $ currentSpeed + initSpeed / 2)
                                                  $ MUA heroAccel now heroSpeed 0
              JumpingHero jump -> let (GS currentSpeed) = muaSpeed (jumpYMvt jump) now
                                      (GS initSpeed) = heroSpeed
                                      (x, y) = jumpPosition jump now in
                                  JumpingHero $ Jump x
                                                     (jumpVx0 jump)
                                                   $ MUA heroAccel
                                                         now 
                                                         (GS $ initSpeed + currentSpeed / 2)
                                                         y
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
    (_heroIdleTexture context
    , (round $ 4 * (now `timeDiff` t0)) `mod` 8
    , x0 - 32
    , winHeight `div` 2)
heroDrawInfo now (RunningHero mua) context =
  let frameCount = 3
      distance = muaDistance mua now
      GS speed = muaSpeed mua now in
    (_heroTexture context
     -- switch from speed based animation to time based to maintain illusion
     -- of movement at low speeds
    , if speed > 5
      then fromIntegral distance `mod` frameCount
      else (round $ 5 * (now `timeDiff` muaT0 mua)) `mod` frameCount
    , muaX0 mua + distance - 32
    , winHeight `div` 2)
heroDrawInfo now (JumpingHero jump) context =
  let step = round $ 6 * (now `timeDiff` (muaT0 $ jumpYMvt jump))
      frameCount = 5
      (x, y) = jumpPosition jump now in
    (_heroJumpTexture context
    , abs $ (step `mod` (frameCount * 2 - 2)) - (frameCount - 1)
    , x - 32
    , winHeight `div` 2 - y)


snakeInKillablePosition :: Position -- ^ snake's position
                        -> Position -- ^ hero's position
                        -> Bool
snakeInKillablePosition snakePos heroPos = snakePos - 64 - 32 < heroPos


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
              -> AppContext -- ^ context holding textures to draw
              -> [(Snake, SDL.Texture, Int, Position, Position)] -- ^ (snake, texture, frame, x, y)
snakeDrawInfo _ _ [] _ = []
snakeDrawInfo now sceneOrigin (s:ss) context
  | snakePos < sceneOrigin - 64 = snakeDrawInfo now sceneOrigin ss context
  | snakePos > sceneOrigin + winWidth = []
  | otherwise = oneSnakeDrawInfo s:snakeDrawInfo now sceneOrigin ss context
  where snakePos = snakePosition s now
        snakeY = winHeight `div` 2 + 128 - 64
        oneSnakeDrawInfo (DyingSnake _ _) = (s, _snakeDieTexture context, 0, snakePos, snakeY)
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


drawApp :: MonadIO m => GameTime -> AppState -> AppContext -> m ()
drawApp now
        (AppState isRed _ sceneOrigin heroState _ fpsEst typing _ snakes)
        context@(AppContext { _renderer=renderer, _font=font, _catTexture=catTexture }) = do
  SDL.rendererDrawColor renderer SDL.$= (if isRed then red else green)
  SDL.clear renderer
  SDL.rendererDrawColor renderer SDL.$= black
  SDL.drawLine renderer (SDL.P $ SDL.V2 minScreenPos 0) (SDL.P $ SDL.V2 minScreenPos winHeight)
  SDL.drawLine renderer (SDL.P $ SDL.V2 maxScreenPos 0) (SDL.P $ SDL.V2 maxScreenPos winHeight)
  let y = winHeight `div` 2 + 128 in
    SDL.drawLine renderer (SDL.P $ SDL.V2 0 y) (SDL.P $ SDL.V2 winWidth y)
  let fps = (take 7 $ show fpsEst)
         ++ case heroState of
              IdleHero _ _ -> ""
              RunningHero mua -> " " ++ (take 7 . show $ muaSpeed mua now)
              JumpingHero _ -> " jumping"
  liftIO $ putStrLn $ show now ++ ": FPS = " ++ fps
  case heroState of
    IdleHero _ _ -> return ()
    RunningHero mua ->
      liftIO $ putStrLn $ "  mua = "
                       ++ (show $ muaDistance mua now)
                       ++ " " ++ (show $ muaMaxDistance mua)
                       ++ " x = " ++ (show $ muaDistance mua now + muaX0 mua)
                       ++ " o = " ++ (show $ sceneOrigin)
    JumpingHero _ -> return ()
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
  let (texture, frame, x, y) = heroDrawInfo now heroState context
  mapM_ (drawSnake renderer sceneOrigin font x)
      $ snakeDrawInfo now sceneOrigin snakes context
  let drawRunText toRun = do
        withNonEmptyTexture renderer font black toRun $ \text -> do
          SDL.TextureInfo { SDL.textureWidth = textWidth
                          , SDL.textureHeight = textHeight
                          } <- SDL.queryTexture text
          SDL.copy renderer
                   text
                   Nothing -- use complete texture as source
                 $ Just $ SDL.Rectangle (SDL.P $ SDL.V2 (winWidth - textWidth) 0)
                                      $ SDL.V2 textWidth textHeight
  let drawJumpText toJump = do
        withNonEmptyTexture renderer font black toJump $ \text -> do
          SDL.TextureInfo { SDL.textureWidth = textWidth
                          , SDL.textureHeight = textHeight
                          } <- SDL.queryTexture text
          SDL.copy renderer
                   text
                   Nothing -- use complete texture as source
                 $ Just $ SDL.Rectangle (SDL.P $ SDL.V2 (x - sceneOrigin + (128 - textWidth) `div` 2) $ y - 16)
                                      $ SDL.V2 textWidth textHeight
  case typing of
    Waiting toRun toJump ->
      drawRunText toRun >> drawJumpText toJump
    Transition _ _ _ toRun toJump ->
      drawRunText toRun >> drawJumpText toJump
    Typing done toType _ _ _ ->
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
  -- draw hero last, i.e. on top of all the rest:
  lSDLcopy renderer
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

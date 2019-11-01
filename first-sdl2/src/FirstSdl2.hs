{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- This would have been impossible without
-- https://github.com/palf/haskell-sdl2-examples
--
-- Modifying the different lessons and mashing them together

module Main (main) where

import qualified SDL
import qualified SDL.Font

import Control.Concurrent     (threadDelay)
import Control.Exception      (handle, throw)
import Control.Monad          (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List              (foldl')
import Data.List.NonEmpty     (NonEmpty(..))
import System.Environment     (getArgs)

import AllSDL
import Hero
import Physics
import Keymaps
import Snake


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
                               (HeroTextures heroTexture heroIdleTexture heroJumpTexture)
                               catTexture
                               (SnakeTextures snakeTexture snakeDieTexture)
                               azerty_on_qwerty
  where mkRenderer c = SDL.createRenderer w c SDL.defaultRenderer
        snakes t0 = zipWith (\idx word -> MovingSnake (idx * winWidth + (winWidth * 3 `div` 4)) t0 word)
                            [0..]
                          $ cycle ['d':|"ur", 'd':|"oux", 'm':|"olle", 'r':|"ose"]


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
  , _heroTextures :: HeroTextures
  , _catTexture :: SDL.Texture
  , _snakeTextures :: SnakeTextures
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

maxScreenPos, minScreenPos :: Position
maxScreenPos = 2 * winWidth `div` 3
minScreenPos = winWidth `div` 8


updateAppTime :: GameTime -- ^ time
              -> AppContext -- ^ application context: drawing info needed for collision detection
              -> AppState -- ^ current application state
              -> Maybe (AppState -- ^ new state, Nothing means the game is over
                       , HeroDrawingInfo -- ^ see `heroDrawInfo', needed for collision detection, reusable for drawing
                       , [SnakeDrawingInfo]) -- ^ see `snakeDrawInfo', needed for collision detection, reusable for drawing
updateAppTime now
              context
              s0@(AppState { _lastTicks=t0, _sceneLastMove=sceneLastMove, _sceneOrigin=sceneOrigin, _fpsEst=fps0, _heroState=hero, _typing=typing, _words=wordList, _snakes=snakes })
  | killedByASnake snakeDrawingInfos = Nothing
  | now > t0 = Just $ (s0 { _lastTicks=now
                          , _sceneLastMove=newSceneLastMove
                          , _sceneOrigin=newSceneOrigin
                          , _fpsEst=(pastWeight * fps0 + timeScaling / (fromIntegral $ now - t0))
                                      / (pastWeight + 1)
                          , _heroState=hero'
                          , _typing=typing'
                          , _words=words'
                          , _snakes=snakes'
                          }
                       , heroDrawingInfo
                       , snakeDrawingInfos)
  | otherwise = Just (s0 , heroDrawingInfo , snakeDrawingInfos)
  where pastWeight = 9 -- higher values mean more weight of the past FPS estimates in current estimate
        hero' = case hero of
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
        heroDrawingInfo@(_, _, heroPos, heroY) =
          heroDrawInfo now hero' (_heroTextures context) $ winHeight `div` 2
        snakeDrawingInfos =
          snakeDrawInfo now newSceneOrigin snakes' $ _snakeTextures context
        killedByASnake = foldr (\s t -> killedByThisSnake s || t) False
        killedByThisSnake (DyingSnake _ _, _, _, _, _) = False
        killedByThisSnake ((MovingSnake _ _ _), _, _, snakeX, snakeY) =
          touchesHero snakeX snakeY
        touchesHero snakeX snakeY = heroRectangle `intersectRectangle` snakeRectangle
          where heroRectangle = SDL.Rectangle (SDL.P $ SDL.V2 heroPos heroY) $ SDL.V2 128 128
                snakeRectangle = SDL.Rectangle (SDL.P $ SDL.V2 snakeX snakeY) $ SDL.V2 64 64
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
                                 $ now + (round $ 2 * timeScaling)):tl
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
  case foldl' (\mbS e -> mbS >>= updateAppForEvent e context) (Just oldState) events of
    Nothing -> return ()
    Just s1 -> do
      now <- SDL.ticks
      case updateAppTime now context s1 of
        Nothing -> return ()
        Just (nextState, heroInfo, snakeInfos) -> do
          drawApp now heroInfo snakeInfos nextState context
          when (_fpsEst nextState > 100) $
            liftIO $ threadDelay $ 10 * 1000 -- microseconds
          appLoop nextState context


drawApp :: MonadIO m
        => GameTime -- ^ time for which to draw
        -> HeroDrawingInfo -- ^ how to draw hero
        -> [SnakeDrawingInfo] -- ^ how to draw snakes
        -> AppState -- ^ current application state
        -> AppContext -- ^ application graphic context
        -> m ()
drawApp now
        (texture, frame, heroX, heroY)
        snakeDrawingInfos
        (AppState isRed _ sceneOrigin heroState _ fpsEst typing _ _)
        (AppContext { _renderer=renderer, _font=font, _catTexture=catTexture }) = do
  SDL.rendererDrawColor renderer SDL.$= (if isRed then red else green)
  SDL.clear renderer
  SDL.rendererDrawColor renderer SDL.$= black
  SDL.drawLine renderer (SDL.P $ SDL.V2 minScreenPos 0) (SDL.P $ SDL.V2 minScreenPos winHeight)
  SDL.drawLine renderer (SDL.P $ SDL.V2 maxScreenPos 0) (SDL.P $ SDL.V2 maxScreenPos winHeight)
  let y = winHeight `div` 2 + 128 in
    SDL.drawLine renderer (SDL.P $ SDL.V2 0 y) (SDL.P $ SDL.V2 winWidth y)
  let fps = take 7 $ show fpsEst
  when (fpsEst > 25) $ do
    withStringTexture renderer font (if isRed then blue else black) fps $ \fpsTexture -> do
      SDL.TextureInfo { SDL.textureWidth = textWidth
                      , SDL.textureHeight = textHeight
                      } <- SDL.queryTexture fpsTexture
      SDL.copy renderer
               fpsTexture
               Nothing -- use complete fpsTexture as source
             $ Just $ SDL.Rectangle (SDL.P $ SDL.V2 0 $ winHeight - textHeight)
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
  mapM_ (drawSnake renderer sceneOrigin font heroX) snakeDrawingInfos
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
                 $ Just $ SDL.Rectangle (SDL.P $ SDL.V2 (heroX - sceneOrigin + (128 - textWidth) `div` 2)
                                                      $ heroY - 16)
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
         $ Just $ SDL.Rectangle (SDL.P $ SDL.V2 (heroX - sceneOrigin) heroY) $ SDL.V2 128 128
  SDL.present renderer


main :: IO ()
main = do
  withSDL $ do
    getArgs >>= \case
      [] -> putStrLn "Second demo only with a font...\n\
                     \cabal new-run first-sdl2 \"$(fc-match --format \"%{file}\")\""
      [fontPath] ->
        withSDLFont fontPath 72 $ \font ->
          withWindow "Typing hero"
                     (fromIntegral winWidth, fromIntegral winHeight)
                   $ win2 font
      _ -> putStrLn "Only one font allowed"

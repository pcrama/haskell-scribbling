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
import Data.List.NonEmpty     (NonEmpty(..), nonEmpty)
import Data.Maybe             (mapMaybe)
import System.Environment     (getArgs)
import System.Random          (getStdRandom, randomR)

import AllSDL
import AtLeast2
import Hero
import Physics
import Keymaps
import Snake


-- | The words the user needs to type to run/jump etc are all at least 2 characters long.
type Word2 = AtLeast2 Char


win2 :: MonadIO m => SDL.Font.Font -> SDL.Window -> m ()
win2 font w = do
    renderer <- liftIO $ handle ((\e -> do
                                    backupRendererIndex <- getSoftwareRendererIndex
                                    case backupRendererIndex of
                                      Just i -> putStrLn (show e ++ ", retrying with " ++ show i) >> mkRenderer i
                                      Nothing -> putStrLn "Can't find backup driver" >> throw e
                                 ) :: SDL.SDLException -> IO SDL.Renderer)
                              $ mkRenderer (-1)
    mbAllWords <- loadAllWords
    case mbAllWords of
      Nothing -> liftIO $ putStrLn "At least one word is shorter than 2 or not enough words"
      Just allWords -> do
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
                runWord <- fmap (either id id) $ pickWord allWords []
                jumpWord <- fmap (either id id) $ pickWord allWords [runWord]
                appLoop (AppState False
                                  startTicks
                                  0
                                  (IdleHero startTicks minScreenPos)
                                  startTicks
                                  0.0
                                  (Waiting runWord jumpWord)
                                  (startTicks + (round $ 10 * timeScaling))
                                  0
                                  0
                                  -- start without snakes, they will be spawned as needed to maintain a
                                  -- `stable' population
                                  []
                                  [])
                      $ AppContext renderer
                                   font
                                   (HeroTextures heroTexture heroIdleTexture heroJumpTexture)
                                   catTexture
                                   (SnakeTextures snakeTexture snakeDieTexture)
                                   azerty_on_qwerty
                                   allWords
  where mkRenderer c = SDL.createRenderer w c SDL.defaultRenderer


data AppState = AppState {
  _isRed :: Bool
  , _sceneLastMove :: GameTime
  , _sceneOrigin :: Position
  , _heroState :: Hero
  , _lastTicks :: GameTime
  , _fpsEst :: Double
  , _typing :: TypingState
  , _nextWordChange :: GameTime
  , _runWordUsage :: Int
  , _jumpWordUsage :: Int
  , _snakes :: [Snake]
  , _previousSnakeDrawingInfo :: [SnakeDrawingInfo] -- ^ see `snakeDrawInfo', needed for collision detection, drawing, making lists of currently killable snakes
  }


data AppContext = AppContext {
  _renderer :: SDL.Renderer
  , _font :: SDL.Font.Font
  , _heroTextures :: HeroTextures
  , _catTexture :: SDL.Texture
  , _snakeTextures :: SnakeTextures
  , _keymap :: SDL.Keycode -> Maybe Char
  , _allWords :: NonEmpty Word2
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
  Waiting Word2 Word2 -- ^ type to run, type to jump
  | Transition GameTime Word2 Word2 Word2 Word2 -- ^ when to switch, old run, old jump, new run, new jump
  | Typing (NonEmpty Char) (NonEmpty Char) Word2 Word2 (GameTime -> Word2 -> Word2 -> AppState -> Maybe AppState) -- ^ already typed, still to type, type to run to fall back to Waiting, type to jump to fall back to Waiting, continuation once the word is typed in full

maxScreenPos, minScreenPos :: Position
maxScreenPos = 2 * winWidth `div` 3
minScreenPos = winWidth `div` 8


wordsInUse :: TypingState -- ^ words for interacting with hero
           -> [Snake] -- ^ conservative: pass all snakes (even if they're not visible yet) to exclude duplicate first letter in the future
           -> [Word2] -- ^ list of words to exclude from choice of new words not yet in the game
wordsInUse typing snakes = heroWords ++ snakeWords
  where heroWords = case typing of
          Waiting r j -> [r, j]
          Transition _ r1 j1 r2 j2 -> [r1, j1, r2, j2]
          Typing _ _ r j _ -> [r, j]
        snakeWords = mapMaybe wordOfSnake snakes
        wordOfSnake (MovingSnake _ _ w) = Just w
        wordOfSnake (DyingSnake _ _) = Nothing


-- | Grow population of snakes if needed to always have a minimum number
--   of snakes in the game (even if they are outside the viewport for some
--   time)
spawnSnake :: MonadIO m
           => GameTime -- ^ time
           -> AppContext -- ^ application context
           -> AppState -- ^ current application state
           -> m [Snake] -- ^ updated snake list
spawnSnake _ _ (AppState { _snakes=s@(_:_:_:_) }) = return s -- there are already enough snakes
spawnSnake now (AppContext { _allWords=w }) (AppState { _typing=t, _snakes=s, _sceneOrigin=sceneOrigin }) =
  let exclude = wordsInUse t s
      minSnakePos = sceneOrigin + winWidth + snakeWidth -- place new snake outside viewport
      startPos = case s of
                   [] -> minSnakePos
                   _ -> max minSnakePos
                          $ maximum (map (flip snakePosition now) s) + (winWidth `div` 4) in do
    killSnakeWord <- fmap (either id id) $ pickWord w exclude
    return $ s ++ [MovingSnake startPos now killSnakeWord]


updateAppTime :: Monad m
              => ([Word2] -> m (Either Word2 Word2)) -- ^ action to pick new random words for hero movement
              -> GameTime -- ^ time
              -> AppContext -- ^ application context: drawing info needed for collision detection
              -> AppState -- ^ current application state
              -> m (Maybe (AppState -- ^ new state, Nothing means the game is over
                          , HeroDrawingInfo -- ^ see `heroDrawInfo', needed for collision detection, reusable for drawing
                          ))
updateAppTime chooseNewWord
              now
              context
              s0@(AppState { _lastTicks=t0, _sceneLastMove=sceneLastMove, _sceneOrigin=sceneOrigin, _fpsEst=fps0, _heroState=hero, _typing=typing, _nextWordChange=nextWordChange, _runWordUsage=runWordUsage, _jumpWordUsage=jumpWordUsage, _snakes=snakes })
  | killedByASnake snakeDrawingInfos = return Nothing
  | now > t0 = do
      (typing', nextWordChange', runWordUsage', jumpWordUsage') <- updateHeroMovementWords
      return $ Just $ (s0 { _lastTicks=now
                          , _sceneLastMove=newSceneLastMove
                          , _sceneOrigin=newSceneOrigin
                          , _fpsEst=(pastWeight * fps0 + timeScaling / (fromIntegral $ now - t0))
                                      / (pastWeight + 1)
                          , _heroState=hero'
                          , _typing=typing'
                          , _nextWordChange=nextWordChange'
                          , _runWordUsage=runWordUsage'
                          , _jumpWordUsage=jumpWordUsage'
                          , _snakes=snakes'
                          , _previousSnakeDrawingInfo=snakeDrawingInfos
                          }
                       , heroDrawingInfo)
  | otherwise = return $ Just (s0, heroDrawingInfo)
  where pastWeight = 9 -- higher values mean more weight of the past FPS estimates in current estimate
        hero' = case hero of
                  IdleHero _ _ -> hero
                  RunningHero mua
                    | muaSpeed mua now > speedZero -> hero
                    | otherwise -> IdleHero (muaTimeOfMaxDistance mua)
                                          $ muaX0 mua + muaMaxDistance mua
                  JumpingHero jump ->
                    let maxJumpTime = jumpEndTime jump
                        (currentX, currentY) = jumpPosition jump now
                        maxJumpHeight = winHeight `div` 2 in
                    case (currentY > maxJumpHeight, now >= maxJumpTime) of
                      -- fall back to ground when jumping too high, to
                      -- penalize players trying to jump out of viewport
                      (True, _) -> JumpingHero $ Jump currentX
                                                      speedZero
                                                    $ MUA (GA (0-10)) now (GS 0.01) $ currentY - 1
                      -- Jump is finished:
                      (_, True) -> IdleHero maxJumpTime
                                          $ jumpX0 jump + jumpDistance jump maxJumpTime
                      (_, _) -> hero
        heroDrawingInfo@(_, _, SDL.P (SDL.V2 heroPos _), heroBbox) =
          heroDrawInfo now hero' (_heroTextures context) $ winHeight `div` 2
        snakeDrawingInfos =
          snakeDrawInfo now newSceneOrigin snakes' $ _snakeTextures context
        killedByASnake = foldr (\s t -> killedByThisSnake s || t) False
        killedByThisSnake (DyingSnake _ _, _, _, _, _) = False
        killedByThisSnake ((MovingSnake _ _ _), _, _, _, bbox) =
          touchesHero bbox
        touchesHero snakeBbox = heroBbox `intersectRectangle` snakeBbox
        (newSceneLastMove, newSceneOrigin) =
          case (sceneLastMove + (round $ timeScaling / 4) < now -- update regularly
               , heroPos > sceneOrigin + maxScreenPos -- don't let hero go too far to the right
               , heroPos > sceneOrigin + minScreenPos -- but don't scroll when more or less in middle
               ) of
            (_, True, _) -> (now, heroPos - maxScreenPos)
            (True, False, True) -> (now, sceneOrigin + 1)
            _ -> (sceneLastMove, sceneOrigin)
        updateHeroMovementWords =
          let noChange = (typing, nextWordChange, runWordUsage, jumpWordUsage) in
          case typing of
            Waiting oldRun oldJump
              | now > nextWordChange || (runWordUsage >= 3) || (jumpWordUsage >= 3) -> do
                  let exclude = wordsInUse typing snakes'
                  newRun <- fmap (either id id) $ chooseNewWord exclude
                  newJump <- fmap (either id id) $ chooseNewWord $ newRun:exclude
                  return (Transition (now + (round $ timeScaling / 2))
                                     oldRun oldJump newRun newJump
                         , now + (round $ 10 * timeScaling)
                         , 0
                         , 0)
              | otherwise -> return noChange
            Transition t _ _ newRun newJump
              | now > t ->
                  return (Waiting newRun newJump, nextWordChange, runWordUsage, jumpWordUsage)
              | otherwise -> return noChange
            Typing _ _ _ _ _ -> return noChange
        snakes' = filter stillVisible snakes
        stillVisible snake@(MovingSnake _ _ _) = snakePosition snake now >= sceneOrigin - snakeWidth
        stillVisible (DyingSnake _ timeout) = now <= timeout


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
                  Waiting run@(AtLeast2 { firstAL2 = x }) jump@(AtLeast2 { firstAL2 = j }) ->
                    case lookup True
                              $ (matchChar x, startToType run run jump startToRun
                              ):(matchChar j, startToType jump run jump startToJump
                              ):snakeKiller run jump of
                      Just r -> r
                      Nothing -> noChange
                  Typing already
                         (x:|xs)
                         oldRun
                         oldJump
                         cont -> if matchChar x
                                 then case xs of
                                        [] -> cont now oldRun oldJump s0
                                        (y:ys) -> Just $ s0 { _typing=Typing (already <> (x:|[]))
                                                                             (y:|ys)
                                                                             oldRun
                                                                             oldJump
                                                                             cont
                                                            }
                                 else noChange
                  Transition _
                             oldRun@(AtLeast2 { firstAL2 = x })
                             oldJump@(AtLeast2 { firstAL2 = j })
                             newRun@(AtLeast2 { firstAL2 = y })
                             newJump@(AtLeast2 { firstAL2 = z }) ->
                               case lookup True
                                         $ (matchChar y
                                            , startToType newRun newRun newJump startToRun
                                         ):(matchChar z
                                            , startToType newJump newRun newJump startToJump
                                         ):(snakeKiller newRun newJump
                                            ++ [(matchChar x
                                                , startToType oldRun newRun newJump startToRun)
                                               , (matchChar j
                                                 , startToType oldJump newRun newJump startToJump)]) of
                                 Just r -> r
                                 Nothing -> noChange
  where heroAccel = GA (-3.0)
        heroSpeed = GS (15.0)
        jumpHorizSpeed = GS (5.0)
        noChange = Just s0
        matchChar = eventIsChar keymap e
        snakeKiller r j = let heroPos = heroPosition now $ _heroState s0 in
                            map (\ (t0, w@(AtLeast2 { firstAL2 = s })) ->
                                     (matchChar s, startToType w r j $ startToKill t0))
                              $ killableSnakes heroPos $ _previousSnakeDrawingInfo s0
        startToType :: Word2 -> Word2 -> Word2 -> (GameTime -> Word2 -> Word2 -> AppState -> Maybe AppState) -> Maybe AppState
        startToType (AtLeast2 { firstAL2 = x, secondAL2 = n, restAL2 = xs }) r j f = 
          Just $ s0 { _typing=Typing (x:|[]) (n:|xs) r j f }
        killASnake :: GameTime -> GameTime -> [Snake] -> [Snake]
        killASnake _ _ [] = []
        killASnake lastLetterTimeStamp t0 (s@(MovingSnake _ u0 _):tl)
          | t0 == u0 = (DyingSnake (snakePosition s lastLetterTimeStamp)
                                 $ lastLetterTimeStamp + (round $ 2 * timeScaling)
                       ):tl
          | otherwise = s:killASnake lastLetterTimeStamp t0 tl
        killASnake lastLetterTimeStamp t0 (s@(DyingSnake _ _):tl) =
          s:killASnake lastLetterTimeStamp t0 tl
        startToKill :: GameTime -> GameTime -> Word2 -> Word2 -> AppState -> Maybe AppState
        startToKill snakeBirth lastLetterTimeStamp run jump s@(AppState { _snakes=snakes0 }) = Just $ s {
          _typing=Waiting run jump
          , _snakes=killASnake lastLetterTimeStamp snakeBirth snakes0
          }
        startToRun :: GameTime -> Word2 -> Word2 -> AppState -> Maybe AppState
        startToRun lastLetterTimeStamp run jump s = Just $ s {
          _typing=Waiting run jump
          , _runWordUsage=1 + _runWordUsage s
          , _heroState=case _heroState s of
              IdleHero _ x0 -> RunningHero $ MUA heroAccel lastLetterTimeStamp heroSpeed x0
              RunningHero mua -> let (GS currentSpeed) = muaSpeed mua lastLetterTimeStamp
                                     (GS initSpeed) = heroSpeed in
                                 RunningHero $ MUA heroAccel
                                                   lastLetterTimeStamp
                                                   (GS $ initSpeed + (currentSpeed / 2))
                                                 $ muaX0 mua + muaDistance mua lastLetterTimeStamp
              JumpingHero _ -> _heroState s -- you can't run if you're already jumping
          }
        startToJump :: GameTime -> Word2 -> Word2 -> AppState -> Maybe AppState
        startToJump lastLetterTimeStamp runWord jumpWord s = Just $ s {
          _typing=Waiting runWord jumpWord
          , _jumpWordUsage=1 + _jumpWordUsage s
          , _heroState=case _heroState s of
              IdleHero _ x0 -> JumpingHero $ Jump x0
                                                  jumpHorizSpeed
                                                $ MUA heroAccel lastLetterTimeStamp heroSpeed 0
              RunningHero mua -> let (GS currentSpeed) = muaSpeed mua lastLetterTimeStamp
                                     (GS initSpeed) = jumpHorizSpeed in
                                 JumpingHero $ Jump (muaX0 mua + muaDistance mua lastLetterTimeStamp)
                                                    (GS $ currentSpeed + initSpeed / 2)
                                                  $ MUA heroAccel lastLetterTimeStamp heroSpeed 0
              JumpingHero jump -> let (GS currentSpeed) = muaSpeed (jumpYMvt jump) lastLetterTimeStamp
                                      (GS initSpeed) = heroSpeed
                                      (x, y) = jumpPosition jump lastLetterTimeStamp in
                                  JumpingHero $ Jump x
                                                     (jumpVx0 jump)
                                                   $ MUA (muaA $ jumpYMvt jump)
                                                         lastLetterTimeStamp 
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
      mbNewState <- updateAppTime (pickWord $ _allWords context) now context s1
      case mbNewState of
        Nothing -> return ()
        Just (nextState, heroInfo) -> do
          drawApp heroInfo nextState context
          when (_fpsEst nextState > 100) $
            liftIO $ threadDelay $ 10 * 1000 -- microseconds
          wordUpdateTime <- SDL.ticks
          newSnakes <- spawnSnake wordUpdateTime context nextState
          appLoop (nextState { _snakes=newSnakes }) context


-- | Draw application state, current time is implicit in HeroDrawingInfo & SnakeDrawingInfo
drawApp :: MonadIO m
        => HeroDrawingInfo -- ^ how to draw hero
        -> AppState -- ^ current application state
        -> AppContext -- ^ application graphic context
        -> m ()
drawApp (texture, frame, SDL.P (SDL.V2 heroX heroY), bbox)
        (AppState isRed _ sceneOrigin _ _ fpsEst typing _ _ _ _ snakeDrawingInfos)
        (AppContext { _renderer=renderer, _font=font, _catTexture=catTexture }) = do
  SDL.rendererDrawColor renderer SDL.$= (if isRed then red else green)
  SDL.clear renderer
  SDL.rendererDrawColor renderer SDL.$= black
  SDL.drawLine renderer (SDL.P $ SDL.V2 minScreenPos 0) (SDL.P $ SDL.V2 minScreenPos winHeight)
  SDL.drawLine renderer (SDL.P $ SDL.V2 maxScreenPos 0) (SDL.P $ SDL.V2 maxScreenPos winHeight)
  let y = winHeight `div` 2 + heroHeight in
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
  let drawHeroTexts toRun toJump = do
        withAtLeast2Texture renderer font black toRun $ \text -> do
          SDL.TextureInfo { SDL.textureWidth = textWidth
                          , SDL.textureHeight = textHeight
                          } <- SDL.queryTexture text
          SDL.copy renderer
                   text
                   Nothing -- use complete texture as source
                 $ Just $ SDL.Rectangle (SDL.P $ SDL.V2 (winWidth - textWidth) 0)
                                      $ SDL.V2 textWidth textHeight
        withAtLeast2Texture renderer font black toJump $ \text -> do
          SDL.TextureInfo { SDL.textureWidth = textWidth
                          , SDL.textureHeight = textHeight
                          } <- SDL.queryTexture text
          lSDLcopy renderer
                   text
                   Nothing -- use complete texture as source
                 $ Just $ SDL.Rectangle (SDL.P $ SDL.V2 (heroX - sceneOrigin + (heroWidth - textWidth) `div` 2)
                                                      $ heroY - textHeight + heroHeight `div` 2)
                                      $ SDL.V2 textWidth textHeight
  case typing of
    Waiting toRun toJump -> drawHeroTexts toRun toJump
    Transition _ _ _ toRun toJump -> drawHeroTexts toRun toJump
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
  SDL.copy renderer
           texture
           (Just $ SDL.Rectangle (SDL.P $ SDL.V2 (fromIntegral frame * tileWidth) 0) $ SDL.V2 tileWidth tileHeight)
         $ Just $ SDL.Rectangle (SDL.P $ SDL.V2 (heroX - sceneOrigin) heroY) $ SDL.V2 heroWidth heroHeight
  let (SDL.Rectangle (SDL.P (SDL.V2 bbX bbY)) dim) = bbox in
    drawRectangle renderer (SDL.Rectangle (SDL.P (SDL.V2 (bbX - sceneOrigin) bbY)) dim)
  SDL.present renderer


-- | Return list of words that do not start with the same letter as any other word in the 2nd list
wordCandidates :: NonEmpty Word2 -> [Word2] -> [Word2]
wordCandidates dict exclude = foldr dropIfCommon1stLetter [] dict
  where firstLetters = map (fst . unCons) exclude
        dropIfCommon1stLetter w@(AtLeast2 { firstAL2 = c }) tl
          | c `elem` firstLetters = tl
          | otherwise = w:tl


pickWord :: MonadIO m => NonEmpty Word2 -> [Word2] -> m (Either Word2 Word2)
pickWord dict@(firstWord:|_) exclude
  | null candidates = return $ Left firstWord
  | otherwise = fmap Right $ selectRandom candidates
  where candidates = wordCandidates dict exclude
        selectRandom xs = do
          idx <- liftIO $ getStdRandom $ randomR (0, length xs - 1)
          return $ xs !! idx


loadAllWords :: MonadIO m => m (Maybe (NonEmpty Word2))
loadAllWords = pure $ traverse atLeast2 [
  "belle"
  , "beurre"
  , "bible"
  , "boris"
  , "cercle"
  , "cil"
  , "coca"
  , "coudre"
  , "dense"
  , "docile"
  , "dorloter"
  , "ectoplasme"
  , "flou"
  , "folle"
  , "fou"
  , "four"
  , "froid"
  , "gentil"
  , "gourde"
  , "gout"
  , "gouverne"
  , "gulliver"
  , "histoire"
  , "hiver"
  , "hurler"
  , "ici"
  , "joie"
  , "jolie"
  , "jouer"
  , "jules"
  , "lecteur"
  , "lent"
  , "leurre"
  , "libre"
  , "liseuse"
  , "loi"
  , "lollipops"
  , "lustre"
  , "moche"
  , "moi"
  , "moine"
  , "molle"
  , "moteur"
  , "mou"
  , "noisette"
  , "nord"
  , "nouvelle"
  , "nuit"
  , "ordre"
  , "origine"
  , "ouistiti"
  , "pile"
  , "piller"
  , "pilule"
  , "pope"
  , "poulpe"
  , "rigolo"
  , "rigueur"
  , "rire"
  , "roue"
  , "rouge"
  , "roule"
  , "ruse"
  , "sortir"
  , "souffler"
  , "soupir"
  , "sourire"
  , "sous"
  , "sur"
  , "surf"
  , "tente"
  , "titre"
  , "tout"
  , "trop"
  , "trouver"
  , "tuile"
  , "ursule"
  , "user"
  , "utile"
  , "vigile"
  , "vigueur"
  , "virgule"
  , "viser"
  , "vue"
  , "yoyo"
  , "zoologie"
  ] >>= nonEmpty


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

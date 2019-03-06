import UI.NCurses
import Control.Monad (forM_)
import Control.Monad.State (
    MonadState(..)
  , StateT
  , evalStateT
  , get
  , lift
  , put
  )
import Data.List (findIndices)
import Data.Maybe (catMaybes)

data Direction = Ri | Up | Le | Do
  deriving (Show, Eq)

data Occupied = CrateOnFree | CrateOnTarget
  deriving (Show, Eq)

data Free = Free | Target
  deriving (Show, Eq)

data Tile = F Free | Wall | O Occupied
  deriving (Show, Eq)

type Pos = (Int, Int)

data Map = Map {
    _rows :: Int
  , _cols :: Int
  , _moveMap :: Pos -> Tile
  , _player :: Pos
  , _undosLeft :: Int
  , _undo :: Maybe Map }

unconstrainedMove :: Pos -> Direction -> Pos
unconstrainedMove (x, y) Ri = (x + 1, y)
unconstrainedMove (x, y) Up = (x, y - 1)
unconstrainedMove (x, y) Le = (x - 1, y)
unconstrainedMove (x, y) Do = (x, y + 1)

tile :: Map -> Pos -> Tile
tile = ($) . _moveMap

move :: Map -> Direction -> Maybe Map
move mp@(Map { _player = p }) dir = case tile mp newPosition of
    F _ -> Just $ mp { _player = newPosition } -- nothing to push, just move
    Wall -> Nothing -- can't walk into wall
    O fromOccupied -> push fromOccupied -- a crate -> see if it can be pushed
  where newPosition = unconstrainedMove p dir
        newCratePosition = unconstrainedMove newPosition dir
        push fromOccupied = case tile mp newCratePosition of
          F toFree -> let movedCrate = moveCrate mp newPosition fromOccupied newCratePosition toFree
                      in Just $ movedCrate { _player = newPosition, _undo = Just mp }
          Wall -> Nothing -- can't push crate through wall
          O _ -> Nothing -- can't push more than 1 crate at a time

moveCrate :: Map -> Pos -> Occupied -> Pos -> Free -> Map
moveCrate mp from fromOccupied to toFree = mp { _moveMap = newMap }
  where newMap p
          | from == p = case fromOccupied of
              CrateOnFree -> F Free
              CrateOnTarget -> F Target
          | to == p = case toFree of
                        Free -> O CrateOnFree
                        Target -> O CrateOnTarget
          | otherwise = tile mp p

won :: Map -> Bool
won mp@(Map { _rows = rows, _cols = cols }) = and [and $ [tile mp (col, row) /= F Target
                                                         | col <- [0..cols - 1]]
                                                  | row <- [0..rows - 1]]

data PlayerCommand = Move Direction | Quit | Pass | Undo

playerTurn :: Monad m => Map -> m PlayerCommand -> m (Maybe Map)
playerTurn mp getCommand = do
  cmd <- getCommand
  case cmd of
    Undo -> case (_undo mp, _undosLeft mp > 0) of
      (Just x, True) -> return $ Just x { _undosLeft = _undosLeft mp - 1 }
      _ -> return $ Just mp -- nothing to undo or undo limit reached
    Quit -> return Nothing
    Move d -> case move mp d of
      Just newMapAndPos -> return $ Just newMapAndPos
      Nothing -> return $ Just mp -- Ignore impossible movement
    Pass -> return $ Just mp

playLevel :: Monad m
          => Map             -- |^ Level to play
          -> m PlayerCommand -- |^ next action requested by player e.g. Move Up
          -> (Map -> m ())   -- |^ draw complete level state
          -> m Bool          -- |^ True if player solved the puzzle, i.e. all targets occupied
playLevel mp getCmd draw = do
  draw mp
  if won mp
  then return True
  else do
    mbMpP <- playerTurn mp getCmd
    case mbMpP of
      Just newMap -> playLevel newMap getCmd draw
      Nothing -> return False

playGame :: Monad m
         => (Bool -> m (Maybe Map)) -- |^ next level (Nothing quits)
         -> m PlayerCommand         -- |^ next action requested by player, e.g. Move Up
         -> (Map -> m ())           -- |^ draw complete level state
         -> (String -> m Bool)      -- |^ prompt for a y/n response
         -> m ()
playGame nextLevel getCmd draw prompt = loop False
  where loop advanceBeforeSelection = do
          mbLevel <- nextLevel advanceBeforeSelection
          case mbLevel of
            Just level -> do
              r <- playLevel level getCmd draw
              loop r
            Nothing -> return ()

makeMap :: [String] -> Maybe Map
makeMap xs =
  let rows = length xs
      len = length $ head xs
  in case (rows, all (== len) $ map length $ tail xs) of
    (0, _) -> Nothing
    (_, False) -> Nothing
    (_, True) -> do
      let keepRowsWithStar (_rowIdx, colIdxs) = not $ null colIdxs
      let enumerate = zip [0..]
      let starPositions = filter keepRowsWithStar $ enumerate $ map (findIndices (=='*')) xs
      (pCol, pRow) <- case starPositions of
                        [] -> return (1, 1) -- default start position
                        [(r, [c])] -> return (c, r) -- only one star -> pick its coordinates
                        _ -> fail "There should be 0 or 1 '*' in map"
      return $ Map {
        _rows = rows
        , _cols = len
        , _player = (pCol, pRow)
        , _undosLeft = 50
        , _undo = Nothing
        , _moveMap = \(x, y) ->
                       if x < 0 || x >= len || y < 0 || y >= rows
                       then Wall
                       else case (xs !! y) !! x of
                         '#' -> Wall
                         'X' -> O CrateOnFree
                         '_' -> F Target
                         _ -> F Free }

data Zipper a = Zipper [a] a [a]

mkZipper :: [a] -> Maybe (Zipper a)
mkZipper [] = Nothing
mkZipper (x:xs) = Just $ Zipper [] x xs

zipperPrev :: Zipper a -> Maybe (Zipper a)
zipperPrev (Zipper [] _ _) = Nothing
zipperPrev (Zipper (p:ps) f ns) = Just $ Zipper ps p $ f:ns

zipperNext :: Zipper a -> Maybe (Zipper a)
zipperNext (Zipper _ _ []) = Nothing
zipperNext (Zipper ps f (n:ns)) = Just $ Zipper (f:ps) n ns

zipperFirst :: Zipper a -> Zipper a
zipperFirst z@(Zipper [] _ _) = z
zipperFirst (Zipper (p:ps) f ns) = zipperFirst $ Zipper ps p $ f:ns

zipperLast :: Zipper a -> Zipper a
zipperLast z@(Zipper _ _ []) = z
zipperLast (Zipper ps f (n:ns)) = zipperLast $ Zipper (f:ps) n ns

zipperFocus :: Zipper a -> a
zipperFocus (Zipper _ f _) = f

data SelectCommand =
  FirstElt | PrevElt | NextElt | LastElt | ConfirmSelection | QuitSelection

selectLevel :: (Monad m, MonadState s m)
            => (s -> Zipper a)      -- |^ extract Zipper of levels from state (there's a lens hiding here)
            -> (Zipper a -> s -> s) -- |^ update state with modified Zipper of levels (there's a lens hiding here)
            -> (a -> m ())          -- |^ draw level
            -> m SelectCommand      -- |^ query user for next action (e.g. skip to next level)
            -> Bool                 -- |^ whether to present next level (True) for selection
            -> m (Maybe a)          -- |^ Just selected level or Nothing if user quits
selectLevel extractFromState updateState drawLevel query advanceBeforeSelection = do
    zipper <- fmap extractFromState $ get
    case (advanceBeforeSelection, zipperNext zipper) of
      (True, Just next) -> go next
      (True, Nothing) -> go $ zipperFirst zipper -- wrap around to present new level
      _ -> go zipper
  where update f = get >>= (put . f)
        go zipper = do
          update $ updateState zipper
          zipper <- fmap extractFromState get
          let focus = zipperFocus zipper
          drawLevel focus
          cmd <- query
          case cmd of
            FirstElt -> go $ zipperFirst zipper
            PrevElt -> goPrevNext zipper zipperPrev
            NextElt -> goPrevNext zipper zipperNext
            LastElt -> go $ zipperLast zipper
            ConfirmSelection -> return $ Just focus
            QuitSelection -> return Nothing
        goPrevNext zipper stepper = case stepper zipper of
                                      Nothing -> go zipper
                                      Just z -> go z

waitFor :: Window -> (Event -> Maybe a) -> Curses a
waitFor w p = loop where
  loop = do
    ev <- tryCurses $ getEvent w Nothing
    case ev of
      -- TODO: swallowing all exceptions all the time while waiting for input
      -- seems ignorant.
      Left _ -> loop
      Right Nothing -> loop
      Right (Just ev') -> case p ev' of
        Nothing -> loop
        Just a -> return a

splitOnBlankLines :: [String] -> [[String]]
splitOnBlankLines [] = []
splitOnBlankLines lst = hd:(splitOnBlankLines $ dropWhile mapSeparator tl)
  where (hd, tl) = break mapSeparator lst
        mapSeparator = all (`elem` " \t") -- NB: all p [] == True => empty line is separator, too!

type AppM = StateT (Zipper Map) Curses

main :: IO ()
main = do
    allLevelsText <- readFile "./levels.txt"
    let levels = catMaybes $ map makeMap $ splitOnBlankLines $ lines allLevelsText
    case mkZipper levels of
      Nothing -> putStrLn "No levels found"
      Just z -> do
        _ <- runCurses $ flip evalStateT z $ do
          lift $ setEcho False
          w <- lift $ defaultWindow
          playGame (selectLevel' w)
                   (lift $ getPlayerCommand' w)
                   (lift . drawMap' w "Move with hjkl or arrows, q to select level, u to undo")
                   (lift . getPlayerDecision' w)
        putStrLn "Bye, bye!"
  where getPlayerCommand' :: Window -> Curses PlayerCommand
        getPlayerCommand' w = waitFor w $ flip lookup [
                                  (EventCharacter 'q', Quit)
                                , (EventCharacter 'u', Undo)
                                , (EventCharacter 'h', Move Le)
                                , (EventCharacter 'j', Move Do)
                                , (EventCharacter 'k', Move Up)
                                , (EventCharacter 'l', Move Ri)
                                , (EventSpecialKey KeyLeftArrow, Move Le)
                                , (EventSpecialKey KeyDownArrow, Move Do)
                                , (EventSpecialKey KeyUpArrow, Move Up)
                                , (EventSpecialKey KeyRightArrow, Move Ri)]
        selectLevel' :: Window -> Bool -> AppM (Maybe Map)
        selectLevel' w =
          selectLevel id
                      const
                      (lift . drawMap' w "Select a level (move with 0, <-/h, l/->, $; q quits; y confirms)")
                      (lift $ waitFor w $ flip lookup [(EventCharacter 'q', QuitSelection)
                                                      , (EventCharacter 'y', ConfirmSelection)
                                                      , (EventSpecialKey KeyEnter, ConfirmSelection)
                                                      , (EventCharacter '\n', ConfirmSelection)
                                                      , (EventCharacter 'h', PrevElt)
                                                      , (EventSpecialKey KeyLeftArrow, PrevElt)
                                                      , (EventCharacter 'l', NextElt)
                                                      , (EventSpecialKey KeyRightArrow, NextElt)
                                                      , (EventCharacter '0', FirstElt)
                                                      , (EventCharacter '^', FirstElt)
                                                      , (EventCharacter '_', FirstElt)
                                                      , (EventCharacter '$', LastElt)])
        drawMap' :: Window -> String -> Map -> Curses ()
        drawMap' w s mp = do
          let (x, y) = _player mp
          setCursorMode CursorInvisible
          updateWindow w $ do
            (winRows, winCols) <- windowSize
            clear
            moveCursor 0 0
            drawString s
            let rowOffs = (winRows - (fromIntegral $ _rows mp)) `div` 2
            let colOffs = (winCols - (fromIntegral $ _cols mp)) `div` 2
            let moveCursorRel x y = moveCursor (rowOffs + fromIntegral y) (colOffs + fromIntegral x)
            forM_ [0.._rows mp - 1] $ \row ->
              forM_ [0.._cols mp - 1] $ \col ->
                let drawChar c r g = do
                      moveCursorRel c r
                      drawString $ g:"" in
                case tile mp (col, row) of
                  Wall -> drawChar col row '#'
                  F Target -> drawChar col row '_'
                  F Free -> return ()
                  O CrateOnFree -> drawChar col row 'X'
                  O CrateOnTarget -> drawChar col row 'x'
            moveCursorRel x y
            drawString "*"
            moveCursorRel 0 $ _rows mp
            drawString $ show (_undosLeft mp) ++ " undos left"
          render
        getPlayerDecision' :: Window -> String -> Curses Bool
        getPlayerDecision' w p = do
          updateWindow w $ do
            clear
            moveCursor 0 0
            drawString p
          render
          waitFor w $ flip lookup [ (EventCharacter 'y', True)
                                  , (EventCharacter 'Y', True)
                                  , (EventCharacter 'n', False)
                                  , (EventCharacter 'N', False)]

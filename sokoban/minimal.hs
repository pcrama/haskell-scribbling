import UI.NCurses
import Control.Monad (forM_)
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

getPlayerCommand :: IO PlayerCommand
getPlayerCommand = do
  c <- getChar
  return $ case c of
    'a' -> Move Le
    's' -> Move Do
    'w' -> Move Up
    'd' -> Move Ri
    'u' -> Undo
    'q' -> Quit
    _ -> Pass

getPlayerDecision :: String -> IO Bool
getPlayerDecision p = do
  putStr p
  putStrLn " (y/n) "
  c <- getChar
  case c of
    'y' -> return True
    'Y' -> return True
    'n' -> return False
    'N' -> return False
    _ -> getPlayerDecision p

playLevel :: Monad m => Map -> m PlayerCommand -> (Map -> m ()) -> m Bool
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
         => [Map]
         -> m PlayerCommand
         -> (Map -> m ())
         -> (String -> m Bool)
         -> m Bool
playGame [] _ _ _ = return True
playGame allLevels@(mp:otherLevels) getCmd draw prompt = do
  result <- playLevel mp getCmd draw
  case (result, null otherLevels) of
    (True, False) -> do
      q <- prompt "Congratulations.  Next level?"
      case q of
        True -> playGame otherLevels getCmd draw prompt
        False -> return True
    (True, True) -> return True
    (False, _) -> do
      q <- prompt "Try this level again?"
      case q of
        True -> playGame allLevels getCmd draw prompt
        False -> return False

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
        , _undosLeft = 5
        , _undo = Nothing
        , _moveMap = \(x, y) ->
                       if x < 0 || x >= len || y < 0 || y >= rows
                       then Wall
                       else case (xs !! y) !! x of
                         '#' -> Wall
                         'X' -> O CrateOnFree
                         '_' -> F Target
                         _ -> F Free }

drawMap :: Map -> IO ()
drawMap mp@(Map { _rows = rows, _cols = cols }) = do
  let pos = _player mp
  forM_ [0..rows - 1] $ \r -> do
    forM_ [0..cols - 1] $ \c -> do
      let spot = (c, r)
      putChar $ case (spot == pos, tile mp spot) of
        -- TODO? (True, Wall) & (True, O _) should be
        -- forbidden.  Are silently ignored here
        (True, _) -> '*'
        (False, F Target) -> '_'
        (False, F Free) -> ' '
        (False, Wall) -> '#'
        (False, O CrateOnTarget) -> 'x'
        (False, O CrateOnFree) -> 'X'
    putChar '\n'

plain :: [Map] -> IO ()
plain levels = do
  r <- playGame levels
                getPlayerCommand
                drawMap
                getPlayerDecision
  putStrLn $ if r then "Congratulations" else "Better luck next time"

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

evalPlayerCommand :: Event -> Maybe PlayerCommand
evalPlayerCommand (EventCharacter 'q') = Just Quit
evalPlayerCommand (EventCharacter 'u') = Just Undo
evalPlayerCommand (EventCharacter 'h') = Just $ Move Le
evalPlayerCommand (EventCharacter 'j') = Just $ Move Do
evalPlayerCommand (EventCharacter 'k') = Just $ Move Up
evalPlayerCommand (EventCharacter 'l') = Just $ Move Ri
evalPlayerCommand (EventSpecialKey KeyLeftArrow) = Just $ Move Le
evalPlayerCommand (EventSpecialKey KeyDownArrow) = Just $ Move Do
evalPlayerCommand (EventSpecialKey KeyUpArrow) = Just $ Move Up
evalPlayerCommand (EventSpecialKey KeyRightArrow) = Just $ Move Ri
evalPlayerCommand _ = Nothing

evalPlayerDecision :: Event -> Maybe Bool
evalPlayerDecision (EventCharacter 'y') = Just True
evalPlayerDecision (EventCharacter 'Y') = Just True
evalPlayerDecision (EventCharacter 'n') = Just False
evalPlayerDecision (EventCharacter 'N') = Just False
evalPlayerDecision _ = Nothing

splitOnBlankLines :: [String] -> [[String]]
splitOnBlankLines [] = []
splitOnBlankLines lst = hd:(splitOnBlankLines $ dropWhile mapSeparator tl)
  where (hd, tl) = break mapSeparator lst
        mapSeparator = all (`elem` " \t") -- NB: all p [] == True => empty line is separator, too!

main :: IO ()
main = do
    allLevelsText <- readFile "./levels.txt"
    let levels = catMaybes $ map makeMap $ splitOnBlankLines $ lines allLevelsText
    r <- runCurses $ do
      setEcho False
      w <- defaultWindow
      playGame levels
               (getPlayerCommand' w)
               (drawMap' w)
               (getPlayerDecision' w)
    putStrLn $ if r then "Congratulations" else "Better luck next time"
  where getPlayerCommand' :: Window -> Curses PlayerCommand
        getPlayerCommand' w = waitFor w evalPlayerCommand
        drawMap' :: Window -> Map -> Curses ()
        drawMap' w mp = do
          let (x, y) = _player mp
          setCursorMode CursorInvisible
          updateWindow w $ do
            (winRows, winCols) <- windowSize
            let rowOffs = (winRows - (fromIntegral $ _rows mp)) `div` 2
            let colOffs = (winCols - (fromIntegral $ _cols mp)) `div` 2
            let moveCursorRel x y = moveCursor (rowOffs + fromIntegral y) (colOffs + fromIntegral x)
            clear
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
          waitFor w evalPlayerDecision

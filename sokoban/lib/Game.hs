module Game (
    Pos(..)
  , Direction(..)
  , Occupied(..)
  , Free(..)
  , Tile(..)
  , Map(..)
  , PlayerCommand(..)
  , makeMap
  , parseLevels
  , playGame
  , tile
  )
where

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

data Pos = Pos { _x :: Int, _y :: Int }
  deriving (Show, Eq)

data Map = Map {
    _rows :: Int
  , _cols :: Int
  , _moveMap :: Pos -> Tile
  , _player :: Pos
  , _undosLeft :: Int
  , _undo :: Maybe Map }

unconstrainedMove :: Pos -> Direction -> Pos
unconstrainedMove p@(Pos { _x = x }) Ri = p { _x = x + 1 }
unconstrainedMove p@(Pos { _y = y }) Up = p { _y = y - 1 }
unconstrainedMove p@(Pos { _x = x }) Le = p { _x = x - 1 }
unconstrainedMove p@(Pos { _y = y }) Do = p { _y = y + 1 }

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
won mp@(Map { _rows = rows, _cols = cols }) = and [and $ [tile mp (Pos { _x = col, _y = row }) /= F Target
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
                        [(r, [c])] -> return (c, r) -- only one star -> pick its coordinates
                        _ -> fail "There should be exactly 1 '*' in map"
      return $ Map {
        _rows = rows
        , _cols = len
        , _player = Pos { _x = pCol, _y = pRow }
        , _undosLeft = 50
        , _undo = Nothing
        , _moveMap = \(Pos { _x = x, _y = y}) ->
                       if x < 0 || x >= len || y < 0 || y >= rows
                       then Wall
                       else case (xs !! y) !! x of
                         '#' -> Wall
                         'X' -> O CrateOnFree
                         '_' -> F Target
                         _ -> F Free }

splitOnBlankLines :: [String] -> [[String]]
splitOnBlankLines [] = []
splitOnBlankLines lst = hd:(splitOnBlankLines $ dropWhile mapSeparator tl)
  where (hd, tl) = break mapSeparator lst
        mapSeparator = all (`elem` " \t") -- NB: all p [] == True => empty line is separator, too!

parseLevels :: String -- |^ All levels together, separated by at least one blank line
            -> [Map]  -- |^ List of levels that parsed OK.  Parse errors are silently dropped from list.
parseLevels = catMaybes . map makeMap . splitOnBlankLines . lines

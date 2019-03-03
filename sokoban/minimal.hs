import Control.Monad (forM_)
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

data Map = Map { _rows :: Int, _cols :: Int, _moveMap :: Pos -> Tile }

unconstrainedMove :: Pos -> Direction -> Pos
unconstrainedMove (x, y) Ri = (x + 1, y)
unconstrainedMove (x, y) Up = (x, y - 1)
unconstrainedMove (x, y) Le = (x - 1, y)
unconstrainedMove (x, y) Do = (x, y + 1)

tile :: Map -> Pos -> Tile
tile = ($) . _moveMap

move :: Map -> Pos -> Direction -> Maybe (Map, Pos)
move mp p dir = case tile mp newPosition of
    F _ -> Just (mp, newPosition) -- nothing to push, just move
    Wall -> Nothing -- can't walk into wall
    O fromOccupied -> push fromOccupied -- a crate -> see if it can be pushed
  where newPosition = unconstrainedMove p dir
        newCratePosition = unconstrainedMove newPosition dir
        push fromOccupied = case tile mp newCratePosition of
          F toFree -> Just (moveCrate mp newPosition fromOccupied newCratePosition toFree
                           , newPosition)
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

data PlayerCommand = Move Direction | Quit | Pass

playerTurn :: Monad m => Map -> Pos -> m PlayerCommand -> m (Maybe (Map, Pos))
playerTurn mp p getCommand = do
  cmd <- getCommand
  case cmd of
    Quit -> return Nothing
    Move d -> case move mp p d of
      Just newMapAndPos -> return $ Just newMapAndPos
      Nothing -> return $ Just (mp, p) -- Ignore impossible movement
    Pass -> return $ Just (mp, p)

getPlayerCommand :: IO PlayerCommand
getPlayerCommand = do
  c <- getChar
  return $ case c of
    'a' -> Move Le
    's' -> Move Do
    'w' -> Move Up
    'd' -> Move Ri
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

playLevel :: Monad m => Map -> Pos -> m PlayerCommand -> (Map -> Pos -> m ()) -> m Bool
playLevel mp p getCmd draw = do
  draw mp p
  if won mp
    then return True
    else do
    mbMpP <- playerTurn mp p getCmd
    case mbMpP of
      Just (newMap, newPos) -> playLevel newMap newPos getCmd draw
      Nothing -> return False

playGame :: Monad m
         => [(Map, Pos)]
         -> m PlayerCommand
         -> (Map -> Pos -> m ())
         -> (String -> m Bool)
         -> m Bool
playGame [] _ _ _ = return True
playGame allLevels@((mp, startPos):otherLevels) getCmd draw prompt = do
  result <- playLevel mp startPos getCmd draw
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
    (_, True) -> Just $
      Map {
        _rows = rows
        , _cols = len
        , _moveMap = \(x, y) ->
                       if x < 0 || x >= len || y < 0 || y >= rows
                       then Wall
                       else case (xs !! y) !! x of
                         '#' -> Wall
                         'X' -> O CrateOnFree
                         '_' -> F Target
                         _ -> F Free }

drawMap :: Map -> Pos -> IO ()
drawMap mp@(Map { _rows = rows, _cols = cols }) pos = do
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

main :: IO ()
main = do
  let levels = catMaybes $ map makeMap [
        ["###############"
        ,"# #  X  _     #"
        ,"#             #"
        ,"# X        #  #"
        ,"# X       _#_ #"
        ,"###############"]
       ,["##############################"
        ,"#   #       #       #        #"
        ,"#   #   X   #   X   #   X    #"
        ,"#   #       #       #        #"
        ,"#   #   #   #   #   #   #   _#"
        ,"#       #       #       #  _ #"
        ,"#   X   #   X   #   X   # _ _#"
        ,"#       #       #       #_ _ #"
        ,"##############################"]]
  r <- playGame (map (\lvl -> (lvl, (1, 1))) levels)
                getPlayerCommand
                drawMap
                getPlayerDecision
  putStrLn $ if r then "Congratulations" else "Better luck next time"



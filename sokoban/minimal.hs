import Control.Monad (forM_)

data Direction = Ri | Up | Le | Do
  deriving (Show, Eq)

data Tile = Free | Wall | Crate
  deriving (Show, Eq)

type Pos = (Int, Int)

data Map = Map { _rows :: Int, _cols :: Int, _moveMap :: Pos -> Tile, _targets :: [Pos] }

unconstrainedMove :: Pos -> Direction -> Pos
unconstrainedMove (x, y) Ri = (x + 1, y)
unconstrainedMove (x, y) Up = (x, y - 1)
unconstrainedMove (x, y) Le = (x - 1, y)
unconstrainedMove (x, y) Do = (x, y + 1)

tile :: Map -> Pos -> Tile
tile = ($) . _moveMap

move :: Map -> Pos -> Direction -> Maybe (Map, Pos)
move mp p dir = case tile mp newPosition of
    Free -> Just (mp, newPosition)
    Wall -> Nothing
    Crate -> case tile mp newCratePosition of
      Free -> Just (moveTile mp newPosition newCratePosition, newPosition)
      Wall -> Nothing
      Crate -> Nothing
  where newPosition = unconstrainedMove p dir
        newCratePosition = unconstrainedMove newPosition dir

moveTile :: Map -> Pos -> Pos -> Map
moveTile mp from to = mp { _moveMap = newMap }
  where newMap p
          | from == p = Free
          | to == p = tile mp from
          | otherwise = tile mp p

won :: Map -> Bool
won map_@Map { _targets = _targets } = and $ map occupiedByCrate _targets
  where occupiedByCrate pos = tile map_ pos == Crate


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
    'h' -> Move Le
    'j' -> Move Do
    'k' -> Move Up
    'l' -> Move Ri
    'q' -> Quit
    _ -> Pass

playLevel :: Map -> Pos -> IO Bool
playLevel mp p = do
  drawMap mp p
  if won mp
    then return True
    else do
    mbMpP <- playerTurn mp p getPlayerCommand
    case mbMpP of
      Just (newMap, newPos) -> playLevel newMap newPos
      Nothing -> return False

enumerate :: [x] -> [(Int, x)]
enumerate = go 0
  where go _ [] = []
        go i (x:xs) = (i, x):go (i + 1) xs

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
                         'X' -> Crate
                         _ -> Free
        , _targets = [(x, y)
                     | (y, row) <- enumerate xs
                     , (x, ch) <- enumerate row
                     , ch == '_'] }

drawMap :: Map -> Pos -> IO ()
drawMap mp@(Map { _targets = targets, _rows = rows, _cols = cols }) pos = do
  forM_ [0..rows - 1] $ \r -> do
    forM_ [0..cols - 1] $ \c -> do
      let spot = (c, r)
      putChar $ case (spot == pos, spot `elem` targets, tile mp spot) of
        -- TODO? (True, _, Wall) & (True, _, Crate) should be
        -- forbidden.  Are silently ignored here
        (True, _, _) -> '*'
        (False, True, Free) -> '_'
        (False, False, Free) -> ' '
        (False, _, Wall) -> '#'
        (False, True, Crate) -> 'x'
        (False, False, Crate) -> 'X'
    putChar '\n'

main :: IO ()
main = do
  let Just mp = makeMap ["##########"
                        ,"# #  X  _#"
                        ,"# X    # #"
                        ,"#     _# #"
                        ,"##########"]
  r <- playLevel mp (1, 1)
  putStrLn $ if r then "Congratulations" else "Better luck next time"



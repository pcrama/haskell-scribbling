import UI.NCurses
import Data.Semigroup ((<>))
import Control.Monad (forM_)
import Control.Monad.State (
    StateT
  , evalStateT
  , lift
  )
import Options.Applicative

import Lib (
    Occupied(..)
  , Direction(..)
  , Free(..)
  , Tile(..)
  , tile
  , Pos(..)
  , Map(..)
  , PlayerCommand(..)
  , playGame
  , mkZipper
  , Zipper
  , SelectCommand(..)
  , interactiveSelect
  , parseLevels
  )

data Opts = Opts {
  maxUndo :: !Int
  , levelsPath :: !FilePath
  }

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

type AppM = StateT (Zipper Map) Curses

main :: IO ()
main = do
    opts <- execParser optsParser
    allLevelsText <- readFile $ levelsPath opts
    case mkZipper $ parseLevels (maxUndo opts) allLevelsText of
      Nothing -> putStrLn "No levels found"
      Just z -> do
        _ <- runCurses $ flip evalStateT z $ do
          lift $ setEcho False
          w <- lift $ defaultWindow
          c <- lift $ setCursorMode CursorInvisible
          playGame (selectLevel' w)
                   (lift $ getPlayerCommand' w)
                   (lift . drawMap' w "Move with hjkl or arrows, q to select level, u to undo")
                   (lift . getPlayerDecision' w)
          lift $ setCursorMode c
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
          interactiveSelect id
                            const
                            (lift
                             . drawMap' w "Select a level (move with 0, <-/h, l/->, $; q quits; y confirms)")
                            (lift
                             $ waitFor w
                             $ flip lookup [(EventCharacter 'q', QuitSelection)
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
          let Pos { _x = x, _y = y } = _player mp
          updateWindow w $ do
            (winRows, winCols) <- windowSize
            clear
            moveCursor 0 0
            drawString s
            let rowOffs = (winRows - (fromIntegral $ _rows mp)) `div` 2
            let colOffs = (winCols - (fromIntegral $ _cols mp)) `div` 2
            let moveCursorRel cx cy = moveCursor (rowOffs + fromIntegral cy) (colOffs + fromIntegral cx)
            forM_ [0.._rows mp - 1] $ \row ->
              forM_ [0.._cols mp - 1] $ \col ->
                let drawChar c r g = do
                      moveCursorRel c r
                      drawString $ g:"" in
                case tile mp (Pos { _x = col, _y = row}) of
                  Wall -> drawChar col row '#'
                  F Target -> drawChar col row '_'
                  F Free -> return ()
                  O CrateOnFree -> drawChar col row 'X'
                  O CrateOnTarget -> drawChar col row 'x'
            moveCursorRel x y
            drawString "*"
            moveCursorRel (0 :: Int) $ _rows mp
            drawString $ show (_undosLeft mp) ++ " undos left"
          render
        getPlayerDecision' :: Window -> String -> Curses Bool
        getPlayerDecision' w p = do
          updateWindow w $ do
            moveCursor 0 0
            drawString p
            drawString " (y/n)"
            clearLine
          render
          waitFor w $ flip lookup [ (EventCharacter 'y', True)
                                  , (EventCharacter 'Y', True)
                                  , (EventCharacter 'n', False)
                                  , (EventCharacter 'N', False)]
        optsParser :: ParserInfo Opts
        optsParser = info (helper <*> versionOption <*> programOptions)
                          (fullDesc
                           <> progDesc "Sokoban game"
                           <> header "Clone of the Sokoban game in text mode")
        versionOption :: Parser (a -> a)
        versionOption = infoOption "0.4.0.0"
                                   (long "version" <> short 'v' <> help "Show version") 
        programOptions :: Parser Opts
        programOptions = Opts <$> option auto (long "undos"
                                               <> short 'u'
                                               <> help "How many undos to allow"
                                               <> metavar "UNDOS"
                                               <> value (50 :: Int))
                              <*> strOption (long "levels"
                                             <> short 'l'
                                             <> help "Path to file with level definitions"
                                             <> metavar "FILE"
                                             <> value "levels.txt")

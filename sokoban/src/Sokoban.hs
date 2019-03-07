import UI.NCurses
import Control.Monad (forM_)
import Control.Monad.State (
    StateT
  , evalStateT
  , lift
  )

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
    allLevelsText <- readFile "./levels.txt"
    case mkZipper $ parseLevels allLevelsText of
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
                case tile mp (Pos { _x = col, _y = row}) of
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

module InteractiveSelect (
    SelectCommand(..)
  , interactiveSelect
  )

where

import Control.Monad.State (
    MonadState
  , StateT
  , get
  , put
  )

import Zipper

data SelectCommand =
  FirstElt | PrevElt | NextElt | LastElt | ConfirmSelection | QuitSelection

interactiveSelect :: (Monad m, MonadState s m)
  => (s -> Zipper a)      -- |^ extract Zipper of selectees from state (there's a lens hiding here)
  -> (Zipper a -> s -> s) -- |^ update state with modified Zipper of selectees (there's a lens hiding here)
  -> (a -> m ())          -- |^ draw selectee
  -> m SelectCommand      -- |^ query user for next action (e.g. skip to next selectee)
  -> Bool                 -- |^ if True, skip last confirmed selectee before presenting choice to user
  -> m (Maybe a)          -- |^ Just selectee or Nothing if user quits
interactiveSelect extractFromState updateState drawLevel query advanceBeforeSelection = do
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

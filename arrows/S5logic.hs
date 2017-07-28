-- Based on section 5 of "Programming with arrows" by John Hughes
module S5logic (
    delay
  , nor
)
where

import Control.Monad (liftM)

import S5sim

delay :: (Monad m, Eq a)
      => Time -> Sim m a a -> Sim m a a
delay delay (Sim f) = Sim $ \a -> do
    (b, s) <- f a
    return (b, loop delay s)
  where loop delay (Ready e@(Event t v) s) =
          ready (Event (delay + t) v) $ loop delay s
        loop delay (Lift m) =
          lift $ liftM (loop delay) m
        loop delay (Wait t dflt k) =
          wait t (loop delay dflt) $ loop delay . k

nor :: Monad m => Time -> Sim m (Bool, Bool) Bool
nor t = pure (uncurry $ not . or) >>> nubA >>> delay t

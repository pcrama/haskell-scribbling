-- Based on section 5 of "Programming with arrows" by John Hughes
module S5logic (
    delay
  , nor
  , norSpec
)
where

import Control.Monad (liftM)

import Arrows ((>>>), (***), Arrow, pure)
import S5sim

nor :: Monad m => Time -> Sim m (Bool, Bool) Bool
nor t = delay t $ pure (not . uncurry (||)) >>> nubA

swap :: Arrow a => a (i, j) (j, i)
swap = pure $ \(i, j) -> (j, i)

norSpec ti1 ti2 t = delay t $
       -- ((delay ti1 $ pure id) *** (delay ti2 $ pure id))
       -- is NOT ok: delays by the sum of ti1+ti2
       (case compare ti1 ti2 of
          LT -> delay ti1 $ swap >>> delayFirst (ti2 - ti1) >>> swap
          EQ -> delay ti1 $ pure id
          GT -> delay ti2 $ delayFirst $ ti1 - ti2)
   >>> pure (not . uncurry (||))
   >>> nubA

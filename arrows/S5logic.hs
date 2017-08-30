-- Based on section 5 of "Programming with arrows" by John Hughes
module S5logic (
    delay
  , delay2
  , nor
  , norSpec
  , mux2
  , mux4
)
where

import Control.Monad (liftM)

import Arrows ((>>>), (***), Arrow, first, pure)
import S5sim

nor :: Monad m => Time -> Sim m (Bool, Bool) Bool
nor t = delay t $ pure (not . uncurry (||)) >>> nubA

swap :: Arrow a => a (i, j) (j, i)
swap = pure $ \(i, j) -> (j, i)

delay2 :: Arrow a => Time -> Time -> a (i, j) (i, j)
delay2 ti1 ti2 = case compare ti1 ti2 of
  -- ((delay ti1 $ pure id) *** (delay ti2 $ pure id))
  -- is NOT ok: delays by the sum of ti1+ti2
  LT -> delay ti1 $ swap >>> delayFirst (ti2 - ti1) >>> swap
  EQ -> delay ti1 $ pure id
  GT -> delay ti2 $ delayFirst $ ti1 - ti2

norSpec ti1 ti2 t = delay t $
       delay2 ti1 ti2
   >>> pure (not . uncurry (||))
   >>> nubA

-- t1, t2: delay for each input, t3 additional delay on top of that
mux2 :: Arrow a => Time -> Time -> Time -> a (i, i, Bool) i
mux2 t1 t2 t3 = pure (\(in1, in2, sel) -> ((in1, in2), sel))
            >>> first (delay2 t1 t2)
            >>> pure (\((in1, in2), sel) -> if sel then in2 else in1)
            >>> delay t3

-- t1, t2: delay for each input, t3 additional delay on top of that
mux4 :: Arrow a => Time -> Time -> Time -> a (i, i, i, i, Bool, Bool) i
mux4 t1 t2 t3 = pure (\(in1, in2, in3, in4, sel1, sel2)
                          -> ((in1, in2, sel1), (in3, in4, sel1), sel2))
            >>> mux2 t1 t2 t3
            >>> mux2 t1 t2 t3
            >>> first (delay2 t1 t2)
            >>> pure (\((in1, in2), sel) -> if sel then in2 else in1)
            >>> delay t3

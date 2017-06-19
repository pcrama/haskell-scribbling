-- inspired by "Programming with Arrows" by John Hughes
module Hdl (
  clock,
  edge,
  nor,
  oscilloscope,
  rail0,
  rail1,
  simulate,
  takeThenAppend
)

where

-- Prelude in ghc 7.6.3 does not have this:
-- import Data.Bool (bool)

import Arrows

takeThenAppend :: Int -> Stream a -> Stream a -> Stream a
takeThenAppend 0 _ tl = tl
takeThenAppend n (Cons h hhs) tl =
  Cons h $ takeThenAppend (n - 1) hhs tl

nor :: Arrow a => a (Bool, Bool) Bool
nor = pure $ not . uncurry (||)

simulate :: StreamMap i o -> Int -> Stream i -> [o]
simulate a n = take n
             . streamToInfList
             . flip runStreamMap a

clock :: Int -> Stream Bool
clock n = go n n True
  where go max 0 val = go max max $ not val
        go max count val = Cons val $ go max (count - 1) val

rail0 :: Stream Bool
rail0 = Cons False rail0

rail1 :: Stream Bool
rail1 = Cons True rail1

oneBit :: Bool -> String
oneBit x = if x then "^^^" else "___"

oscilloscope :: [Bool] -> String
oscilloscope [] = ""
oscilloscope [x] = oneBit x
oscilloscope (x:y:xs)
  | x == y = oneBit x ++ (oscilloscope $ y:xs)
  | x == True = "^^\\___" ++ oscilloscope xs
  | x == False = "__/^^^" ++ oscilloscope xs

edge :: ArrowCircuit a => a Bool Bool
edge = idA &&& delay False >>> pure detect
  where detect (True, False) = True
        detect _ = False

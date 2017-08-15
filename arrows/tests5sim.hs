import Arrows (
    (>>>)
  , pure
  )

import S5sim
import S5logic

instance Message IO where
  message = putStrLn

main = runSim (    printA "in"
               >>> norSpec 0.15 0.1 0.5
               >>> printA "out")
              (False, False)
              [ Event 1 (False, True)
              , Event 2 (True, False)
              , Event 3 (True, True)
              , Event 4 (False, False)
              , Event 5 (True, False)
              , Event 6 (False, False)
              , Event 7 (False, True)]

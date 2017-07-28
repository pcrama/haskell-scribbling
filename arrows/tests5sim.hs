import Arrows (
    (>>>)
  , pure
  )
import S5sim

instance Message IO where
  message = putStrLn

main = runSim (    pure (uncurry (&&))
               >>> printA "out"
               >>> nubA
               >>> printA "nubbed")
              (False, False)
              [Event 1 (False, True)
              , Event 2 (True, False)
              , Event 3 (True, True)]

import Arrows
import Hdl

slowClock = clock 7
fastClock = takeThenAppend 7 (clock 2) (clock 3)

maxLen = 58

notA :: Arrow a => a Bool Bool
notA = pure (\x -> (x, x)) >>> nor

setReset :: (ArrowLoop a, ArrowCircuit a)
         => a (Bool, Bool) Bool
setReset = let q = False in loop (ff >>> (delay (q, (q, not q))))
  where crossSwitch = pure $ \((s, r), (q, q')) ->
                              ((r, q'), (s, q))
        outRoute = pure $ \(x, y) -> (x, (x, y))
        ff =     crossSwitch
             >>> first nor
             >>> second nor
             >>> outRoute

-- not tested, but compiles
fullAdder1 :: Arrow a => a (Bool, Bool) (Bool, Bool)
fullAdder1 =
  pure $ \(x, y) -> ( x && y -- carry)
                    , (x && not y) || (not x && y))

-- not tested, but compiles
adderWithCarry :: Arrow a
               => a (Bool, Bool, Bool) (Bool, Bool)
adderWithCarry =
      pure (\(x, y, z) -> ((x, y), z))
  >>> first fullAdder1
  >>> pure (\((c1, s), z) -> (c1, (s, z)))
  >>> second fullAdder1
  >>> pure (\(c1, (c2, s)) -> (c1 || c2, s))

main :: IO ()
main = do
  putStrLn "slow="
  putStrLn . take maxLen $ oscilloscope $ streamToInfList slowClock
  putStrLn "fast="
  putStrLn . take maxLen $ oscilloscope $ streamToInfList fastClock
  putStrLn "slow `nor` fast="
  putStrLn . take maxLen $ oscilloscope $ simulate nor maxLen $ streamZip slowClock fastClock
  putStrLn "not fast="
  putStrLn . take maxLen $ oscilloscope $ simulate notA maxLen fastClock
  putStrLn "setReset slow fast"
  putStrLn . take maxLen $ oscilloscope $ simulate setReset maxLen $ streamZip slowClock fastClock
  putStrLn "setReset fast slow"
  putStrLn . take maxLen $ oscilloscope $ simulate setReset maxLen $ streamZip fastClock slowClock
  putStrLn "edge fast"
  putStrLn . take maxLen $ oscilloscope $ simulate edge maxLen $ fastClock

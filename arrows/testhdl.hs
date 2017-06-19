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

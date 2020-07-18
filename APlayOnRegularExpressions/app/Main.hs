module Main
where

import System.Environment (getArgs)
import Text.Read (readMaybe)

import LibAct2

main :: IO ()
main = do
  args <- getArgs
  case makeRE args of
    Just re -> do
      input <- getLine
      let match = matchS re input
      putStrLn $ if match then "Found" else "NOT FOUND"
    Nothing ->
      putStrLn "\
        \match [a5|a10|a100|a200|a300|a400|a500|a600|a675|a700|a800|a900|a1000|a1400|a1500|a1700|a1800|a <n> a|s <s>]\n\
        \n\
        \Match stdin against (a?){n}a{n}, .*a.{n}a.* or literal string"

makeRE :: [String] -> Maybe (Reg Bool Char)
makeRE = work
  where work ["a5"] = Just $ buildAn 5
        work ["a10"] = Just $ buildAn 10
        work ["a100"] = Just $ buildAn 100
        work ["a200"] = Just $ buildAn 200
        work ["a300"] = Just $ buildAn 300
        work ["a400"] = Just $ buildAn 400
        work ["a500"] = Just $ buildAn 500
        work ["a600"] = Just $ buildAn 600
        work ["a675"] = Just $ buildAn 675
        work ["a700"] = Just $ buildAn 700
        work ["a800"] = Just $ buildAn 800
        work ["a900"] = Just $ buildAn 900
        work ["a1000"] = Just $ buildAn 1000
        work ["a1400"] = Just $ buildAn 1400
        work ["a1500"] = Just $ buildAn 1500
        work ["a1700"] = Just $ buildAn 1700
        work ["a1800"] = Just $ buildAn 1800
        work ["a", ns, "a"] = case readMaybe ns of
                                Just n -> Just $ buildAnA n
                                Nothing -> Nothing
        work ["s", s@(_:_)] = Just $ seqS repAny $ seqS (mxToS $ sequ s) repAny
        work _ = Nothing
        seqn n = foldr1 seqS . replicate n
        a = symS ('a' ==)
        buildAn n = (seqn n $ altS a epsS) `seqS` (seqn n a)
        anyS = symS $ const True
        repAny = repS anyS
        buildAnA n = seqS repAny $ seqS a $ seqS (seqn n anyS) $ seqS a repAny

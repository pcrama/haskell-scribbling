module Main
where

import System.Environment (getArgs)
import Text.Read (readMaybe)

import qualified LibAct2
import LibAct2 (RegMX(..), sym)
import qualified LibAct3

data LibToUse = Two | Three

data RegExpToUse = AqnAn Int
                 | AnA Int
                 | Lit String

main :: IO ()
main = do
  args <- getArgs
  let parsedArgs = (
        case args of
          ("2":_) -> Just Two
          ("3":_) -> Just Three
          _ -> Nothing
        , case args of
            [_, ('a':(ns@(_:_)))] -> fmap AqnAn $ readMaybe ns
            [_, "a", ns, "a"] -> fmap AnA $ readMaybe ns
            [_, "s", s] -> return $ Lit s
            _ -> Nothing)
  case parsedArgs of
    (Just libToUse, Just re) -> do
      let match = (buildMatcher libToUse re :: String -> Bool)
      input <- getLine
      putStrLn $ if match input then "Found" else "NOT FOUND"
    _ ->
      putStrLn "\
        \match [2|3] [a<n>|a <n> a|s <s>]\n\n\
        \Match stdin using implementation from Act 2 or Act 3 against\n\
        \^(a?){n}a{n}$, ^[ab]*a[ab]{n}a[ab]*$ or a literal string."

buildMatcher :: LibToUse -> RegExpToUse -> (String -> Bool)
buildMatcher = work
  where work libToUse (AqnAn n) = matchCompile libToUse $ buildAn n
        work libToUse (AnA n) = matchCompile libToUse $ buildAnA n
        work libToUse (Lit s) = matchWrapCompile libToUse $ foldr1 SeqMX $ map sym s
        seqn n = foldr1 SeqMX . replicate n
        matchCompile Two = LibAct2.matchS . LibAct2.mxToS
        matchCompile Three = LibAct3.matchS . LibAct3.mxToS
        matchWrapCompile Two = LibAct2.matchS . LibAct2.unAnchor . LibAct2.mxToS
        matchWrapCompile Three = LibAct3.matchS . LibAct3.unAnchor . LibAct3.mxToS
        a = sym 'a'
        buildAn n = (seqn n $ a `AltMX` EpsMX) `SeqMX` (seqn n a)
        anyS = a `AltMX` sym 'b'
        repAny = RepMX anyS
        buildAnA n = repAny `SeqMX` a `SeqMX` (seqn n anyS) `SeqMX` a `SeqMX` repAny

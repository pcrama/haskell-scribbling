module Main
where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import Text.Read (readMaybe)

import qualified LibAct2
import LibAct2 (RegMX(..), sym)
import qualified LibAct3

data LibToUse = Two | Three

data RegExpToUse = AqnAn Int
                 | AnA Int
                 | Lit Char String

data StringOrText = AsString | AsText

main :: IO ()
main = do
  args <- getArgs
  let parsedArgs = (
        case args of
          ("2t":_) -> Just (Two, AsText)
          ("3t":_) -> Just (Three, AsText)
          ("2s":_) -> Just (Two, AsString)
          ("3s":_) -> Just (Three, AsString)
          _ -> Nothing
        , case args of
            [_, ('a':(ns@(_:_)))] -> fmap AqnAn $ readMaybe ns
            [_, "a", ns, "a"] -> fmap AnA $ readMaybe ns
            [_, "s", (s:ss)] -> return $ Lit s ss
            _ -> Nothing)
  case parsedArgs of
    (Just (libToUse, AsString), Just re) -> do
      let match = (buildMatcher libToUse re :: String -> Bool)
      input <- getLine
      putStrLn $ if match input then "Found" else "NOT FOUND"
    (Just (libToUse, AsText), Just re) -> do
      let match = (buildTMatcher libToUse re :: T.Text -> Bool)
      input <- TIO.getLine
      putStrLn $ if match input then "Found" else "NOT FOUND"
    _ ->
      putStrLn "\
        \match [2|3][s|t] [a<n>|a <n> a|s <s>]\n\n\
        \Match stdin using implementation from Act 2 or Act 3 against\n\
        \^(a?){n}a{n}$, ^[ab]*a[ab]{n}a[ab]*$ or a literal string."

buildMatcher :: LibToUse -> RegExpToUse -> (String -> Bool)
buildMatcher = work
  where work libToUse (AqnAn n) = matchCompile libToUse $ buildAn libToUse n
        work libToUse (AnA n) = matchCompile libToUse $ buildAnA libToUse n
        work libToUse (Lit s ss) = matchWrapCompile libToUse $ foldr1 SeqMX $ map sym $ s:ss
        seqn Two n x = foldr1 SeqMX $ replicate n x
        seqn Three 0 _ = EpsMX
        seqn Three 1 x = x
        seqn Three n x = (seqn Three half x) `SeqMX` (seqn Three (n - half) x)
          where half = n `div` 2
        matchCompile Two = LibAct2.matchS . LibAct2.mxToS
        matchCompile Three = LibAct3.matchS . LibAct3.mxToS
        matchWrapCompile Two = LibAct2.matchS . LibAct2.unAnchor . LibAct2.mxToS
        matchWrapCompile Three = LibAct3.matchS . LibAct3.unAnchor . LibAct3.mxToS
        a = sym 'a'
        buildAn libToUse n = (seqn libToUse n $ a `AltMX` EpsMX) `SeqMX` (seqn libToUse n a)
        aOrB = a `AltMX` sym 'b'
        repAorB = RepMX aOrB
        buildAnA libToUse n = repAorB `SeqMX` a `SeqMX` (seqn libToUse n aOrB) `SeqMX` a `SeqMX` repAorB

buildTMatcher :: LibToUse -> RegExpToUse -> (T.Text -> Bool)
buildTMatcher = work
  where work libToUse (AqnAn n) = matchCompile libToUse $ buildAn libToUse n
        work libToUse (AnA n) = matchCompile libToUse $ buildAnA libToUse n
        work libToUse (Lit s ss) = matchWrapCompile libToUse $ foldr1 SeqMX $ map sym $ s:ss
        seqn Two n x = foldr1 SeqMX $ replicate n x
        seqn Three 0 _ = EpsMX
        seqn Three 1 x = x
        seqn Three n x = (seqn Three half x) `SeqMX` (seqn Three (n - half) x)
          where half = n `div` 2
        matchCompile Two = LibAct2.tmatchS . LibAct2.mxToS
        matchCompile Three = LibAct3.tmatchS . LibAct3.mxToS
        matchWrapCompile Two = LibAct2.tmatchS . LibAct2.unAnchor . LibAct2.mxToS
        matchWrapCompile Three = LibAct3.tmatchS . LibAct3.unAnchor . LibAct3.mxToS
        a = sym 'a'
        buildAn libToUse n = (seqn libToUse n $ a `AltMX` EpsMX) `SeqMX` (seqn libToUse n a)
        aOrB = a `AltMX` sym 'b'
        repAorB = RepMX aOrB
        buildAnA libToUse n = repAorB `SeqMX` a `SeqMX` (seqn libToUse n aOrB) `SeqMX` a `SeqMX` repAorB

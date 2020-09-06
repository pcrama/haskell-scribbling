module Main
where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import Text.Read (readMaybe)

import qualified LibAct2
import LibAct2 (RegMX(..), sym)
import qualified LibAct3
import qualified LibOwn
import Semiring (Semiring)

data LibToUse = Two -- LibAct2
              | Three -- LibAct3
              | Own -- LibOwn
              | PrePost -- LibOwn with PreMX/PostMX wrapping all regular expressions

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
          ("ot":_) -> Just (Own, AsText)
          ("os":_) -> Just (Own, AsString)
          ("pt":_) -> Just (PrePost, AsText)
          ("ps":_) -> Just (PrePost, AsString)
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
        \match [2|3|o|p][s|t] [a<n>|a <n> a|s <s>]\n\n\
        \Match stdin using implementation from Act 2, Act 3 or own extensions against\n\
        \^(a?){n}a{n}$, ^[ab]*a[ab]{n}a[ab]*$ or a literal string."

buildMatcher :: LibToUse -> RegExpToUse -> (String -> Bool)
buildMatcher = work
  where work libToUse (AqnAn n) = mc libToUse $ buildAn libToUse n
        work libToUse (AnA n) = mc libToUse $ buildAnA libToUse n
        work libToUse (Lit s ss) = mwc libToUse $ foldr1 SeqMX $ map sym $ s:ss
        mc libToUse = matchCompile libToUse LibAct2.matchS LibAct3.matchS LibOwn.matchS
        mwc libToUse = matchWrapCompile libToUse LibAct2.matchS LibAct3.matchS LibOwn.matchS
        a = sym 'a'
        buildAn libToUse n = (seqn libToUse n $ a `AltMX` EpsMX) `SeqMX` (seqn libToUse n a)
        aOrB = a `AltMX` sym 'b'
        repAorB = RepMX aOrB
        buildAnA libToUse n = repAorB `SeqMX` a `SeqMX` (seqn libToUse n aOrB) `SeqMX` a `SeqMX` repAorB

buildTMatcher :: LibToUse -> RegExpToUse -> (T.Text -> Bool)
buildTMatcher = work
  where work libToUse (AqnAn n) = mc libToUse $ buildAn libToUse n
        work libToUse (AnA n) = mc libToUse $ buildAnA libToUse n
        work libToUse (Lit s ss) = mwc libToUse $ foldr1 SeqMX $ map sym $ s:ss
        mc libToUse = matchCompile libToUse LibAct2.tmatchS LibAct3.tmatchS LibOwn.tmatchS
        mwc libToUse = matchWrapCompile libToUse LibAct2.tmatchS LibAct3.tmatchS LibOwn.tmatchS
        a = sym 'a'
        buildAn libToUse n = (seqn libToUse n $ a `AltMX` EpsMX) `SeqMX` (seqn libToUse n a)
        aOrB = a `AltMX` sym 'b'
        repAorB = RepMX aOrB
        buildAnA libToUse n = repAorB `SeqMX` a `SeqMX` (seqn libToUse n aOrB) `SeqMX` a `SeqMX` repAorB

seqn :: LibToUse -> Int -> RegMX a -> RegMX a
seqn Two n x = foldr1 SeqMX $ replicate n x
seqn _ 0 _ = EpsMX
seqn _ 1 x = x
seqn lib n x = (seqn lib half x) `SeqMX` (seqn lib (n - half) x)
  where half = n `div` 2

prePosterize :: RegMX Char -> LibOwn.RegMX Char
prePosterize LibAct2.EpsMX = LibOwn.EpsMX
prePosterize (LibAct2.SymMX _ c) = LibOwn.PreMX False f $ LibOwn.PostMX (LibOwn.sym c) False g False
  where f _ d = d == c
        g d _ = d == c
prePosterize (LibAct2.AltMX r s) = prePosterize r `LibOwn.AltMX` prePosterize s
prePosterize (LibAct2.SeqMX r s) = prePosterize r `LibOwn.SeqMX` prePosterize s
prePosterize (LibAct2.RepMX r) = LibOwn.RepMX $ prePosterize r

matchCompile :: Semiring s
             => LibToUse
             -> (LibAct2.Reg s Char -> c)
             -> (LibAct3.Reg s Char -> c)
             -> (LibOwn.Reg s Char -> c)
             -> RegMX Char
             -> c
matchCompile Two t2 _ _ = t2 . LibAct2.mxToS
matchCompile Three _ t3 _ = t3 . LibAct3.mxToS
matchCompile Own _ _ to = to . LibOwn.mxToS . LibOwn.libAct2MXtoOwnMX
matchCompile PrePost _ _ to = to . LibOwn.mxToS . prePosterize

matchWrapCompile :: Semiring s
                 => LibToUse
                 -> (LibAct2.Reg s Char -> c)
                 -> (LibAct3.Reg s Char -> c)
                 -> (LibOwn.Reg s Char -> c)
                 -> RegMX Char
                 -> c
matchWrapCompile Two t2 _ _ = t2 . LibAct2.unAnchor . LibAct2.mxToS
matchWrapCompile Three _ t3 _ = t3 . LibAct3.unAnchor . LibAct3.mxToS
matchWrapCompile Own _ _ to = to . LibOwn.unAnchor . LibOwn.mxToS . LibOwn.libAct2MXtoOwnMX
matchWrapCompile PrePost _ _ to = to . LibOwn.unAnchor . LibOwn.mxToS . prePosterize

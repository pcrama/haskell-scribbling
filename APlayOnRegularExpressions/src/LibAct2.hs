module LibAct2 (
  RegMX(..)
  , Reg
  , altS
  , epsS
  , evenCs
  , matchMX
  , matchS
  , mxToS
  , repS
  , seqS
  , sequ
  , shiftS
  , split2
  , sym
  , symS
)
where

import Data.List (foldl')
import Semiring (Semiring(..))

data RegMX a = EpsMX
             | SymMX Bool a
             | AltMX (RegMX a) (RegMX a)
             | SeqMX (RegMX a) (RegMX a)
             | RepMX (RegMX a)
 deriving Show

sym :: a -> RegMX a
sym = SymMX False

split2 :: [a] -> ([a], [a])
split2 [] = ([], [])
split2 [a] = ([a], [])
split2 (a:as@(_:fast)) = go as fast [a]
  where go slow [] acc = (acc, slow)
        go slow [_] acc = (acc, slow)
        go (h:s) (_:_:f) acc = go s f $ acc ++ [h]
        go [] _ _ = error "Not reached: fast list traversal should have got to end of list first"
      
sequ :: [a] -> RegMX a
sequ [] = EpsMX
sequ [a] = sym a
sequ as = SeqMX (sequ lf) (sequ rg)
  where (lf, rg) = split2 as

evenCs :: RegMX Char
evenCs = let aOrB = AltMX (sym 'a') (sym 'b')
             aOrBStar = RepMX aOrB
             c = sym 'c'
             aOrBStarC = SeqMX aOrBStarC c
         in SeqMX (RepMX (SeqMX aOrBStarC aOrBStarC)) aOrBStar

-- True if regular expression matches the empty String
empty :: RegMX a -> Bool
empty EpsMX = True
empty (SymMX _ _) = False
empty (AltMX a b) = empty a || empty b
empty (SeqMX a b) = empty a && empty b
empty (RepMX _) = True

-- True if final character of regular expression contains is matched
final :: RegMX a -> Bool
final EpsMX = False
final (SymMX m _) = m
final (AltMX a b) = final a || final b
final (SeqMX a b) = (final a && empty b) || final b
final (RepMX a) = final a

shift :: Eq a => Bool -> RegMX a -> a -> RegMX a
shift _ EpsMX _ = EpsMX
shift m (SymMX _ c) x = SymMX (m && x == c) c -- previous symbol was marked and this one matches
shift m (AltMX a b) x = AltMX (shift m a x) (shift m b x)
shift m (SeqMX a b) x = SeqMX (shift m a x)
                            $ shift (-- had a match & 1st part of SeqMX matches empty input
                                     (m && empty a)
                                     -- or first part has been matched up to the end
                                     || final a)
                                    b
                                    x
shift m (RepMX a) x = RepMX $ shift (m || final a) a x

matchMX :: Eq a => RegMX a -> [a] -> Bool
matchMX r [] = empty r
matchMX r (a:as) = final $ foldl' (shift False) (shift True r a) as

-- emptyS: True if regular expression matches the empty String
-- finalS: True if final character of regular expression is matched, i.e.
--         if regular expression accepts the empty string as the end of
--         the match
data Reg s a = Reg { emptyS :: s, finalS :: s, regS :: RegS s a }

data RegS s a = EpsS
              | SymS (a -> s)
              | AltS (Reg s a) (Reg s a)
              | SeqS (Reg s a) (Reg s a)
              | RepS (Reg s a)

epsS :: Semiring s => Reg s a
epsS = Reg { emptyS=one, finalS=zero, regS=EpsS }

symS :: Semiring s => (a -> s) -> Reg s a
symS f  = Reg { emptyS=zero, finalS=zero, regS=SymS f }

altS :: Semiring s => Reg s a -> Reg s a -> Reg s a
altS r s = Reg { emptyS = emptyS r `splus` emptyS s
               , finalS = finalS r `splus` finalS s
               , regS = AltS r s
               }

seqS :: Semiring s => Reg s a -> Reg s a -> Reg s a
seqS r s = Reg { emptyS = emptyS r `stimes` emptyS s
               , finalS = (finalS r `stimes` emptyS s) `splus` finalS s
               , regS = SeqS r s
               }
repS :: Semiring s => Reg s a -> Reg s a
repS r@(Reg { finalS = f }) = Reg { emptyS = one, finalS = f, regS = RepS r }

shiftS :: Semiring s => s -> RegS s a -> a -> Reg s a
shiftS _ EpsS _ = epsS
shiftS m (SymS f) x = (symS f) { finalS=m `stimes` f x }
shiftS m (AltS p q) x = altS p' q'
  where p' = shiftS m (regS p) x
        q' = shiftS m (regS q) x
shiftS m (SeqS p q) x = seqS p' q'
  where p' = shiftS m (regS p) x
        q' = shiftS ((m `stimes` emptyS p) `splus` finalS p) (regS q) x
shiftS m (RepS r) x = repS $ shiftS (m `splus` finalS r) (regS r) x

matchS :: Semiring s => Reg s a -> [a] -> s
matchS r [] = emptyS r
matchS r (a:as) = finalS $ foldl' (shiftS zero . regS) (shiftS one (regS r) a) as

mxToS :: (Semiring s, Eq a) => RegMX a -> Reg s a
mxToS EpsMX = epsS
mxToS (SymMX _ a) = symS $ \x -> if x == a then one else zero
mxToS (AltMX a b) = altS (mxToS a) (mxToS b)
mxToS (SeqMX a b) = seqS (mxToS a) (mxToS b)
mxToS (RepMX r) = repS $ mxToS r

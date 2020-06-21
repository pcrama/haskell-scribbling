module LibAct2 (
  RegMX(..)
  , evenCs
  , matchMX
  , sequ
  , split2
  , sym
)
where

import Data.List (foldl')

data RegMX a = EpsMX
             | SymMX Bool a
             | AltMX (RegMX a) (RegMX a)
             | SeqMX (RegMX a) (RegMX a)
             | RepMX (RegMX a)

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

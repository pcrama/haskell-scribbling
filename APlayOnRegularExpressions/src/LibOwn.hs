-- Expand on ideas of LibAct2 & LibAct3
module LibOwn (
  RegMX(..)
  , Reg
  , altS
  , boolToSemiring
  , endOfWordAfter
  , epsS
  , libAct2MXtoOwnMX
  , matchMX
  , matchS
  , mxToS
  , preS
  , postS
  , repS
  , seqS
  , sequ
  , shiftS
  , startOfWordThen
  , submatchW
  , sym
  , symS
  , tmatchS
  , unAnchor
)
where

-- (a|b|-)*(\<a|b)c
--
--                                SeqMX
--                         +--------+---------+
--                       SeqMX             SymMX 'c'
--              +----------+----------+
--            RepMX                 AltMX
--            AltMX              +----+----+
--          +---+---+          PreMX    SymMX 'b'
--        AltMX  SymMX '-'    SymMX 'a'
--    +-----+-----+
-- SymMX 'a'   SymMX 'b'

import Data.Char (isAlphaNum)
import Data.List (foldl')
import qualified Data.Text as T
import LibAct2 (sequ_)
import qualified LibAct2
import Semiring (Semiring(..))

data RegMX a = EpsMX
             | SymMX Bool a
             -- previous ---v          v--- elt
             | PreMX (Maybe a -> Maybe a -> Bool) (RegMX a)
             --       last seen elt --------v          v--- peek next elt
             | PostMX (RegMX a) Bool (Maybe a -> Maybe a -> Bool)
             | AltMX (RegMX a) (RegMX a)
             | SeqMX (RegMX a) (RegMX a)
             | RepMX (RegMX a)

instance Show a => Show (RegMX a) where
  show EpsMX = "EpsMX"
  show (SymMX f a) = "(SymMX " ++ show f ++ " " ++ show a ++ ")"
  show (PreMX _ r) = "(PreMX " ++ " ? " ++ show r ++ ")"
  show (PostMX r p _) = "(PostMX " ++ show r ++ " " ++ show p ++ " ?)"
  show (AltMX r s) = "(AltMX " ++ show r ++ " " ++ show s ++ ")"
  show (SeqMX r s) = "(SeqMX " ++ show r ++ " " ++ show s ++ ")"
  show (RepMX r) = "(RepMX " ++ show r ++ ")"

libAct2MXtoOwnMX :: LibAct2.RegMX a -> RegMX a
libAct2MXtoOwnMX LibAct2.EpsMX = EpsMX
libAct2MXtoOwnMX (LibAct2.SymMX f a) = SymMX f a
libAct2MXtoOwnMX (LibAct2.AltMX r s) = libAct2MXtoOwnMX r `AltMX` libAct2MXtoOwnMX s
libAct2MXtoOwnMX (LibAct2.SeqMX r s) = libAct2MXtoOwnMX r `SeqMX` libAct2MXtoOwnMX s
libAct2MXtoOwnMX (LibAct2.RepMX r) = RepMX $ libAct2MXtoOwnMX r

sym :: a -> RegMX a
sym = SymMX False

sequ :: [a] -> RegMX a
sequ = sequ_ EpsMX sym SeqMX

startOfWordThen :: RegMX Char -> RegMX Char
startOfWordThen = PreMX f
  where f Nothing (Just c) = isAlphaNum c
        f (Just b) (Just c) = (not $ isAlphaNum b) && isAlphaNum c
        f _ Nothing = False

endOfWordAfter :: RegMX Char -> RegMX Char
endOfWordAfter r = PostMX r False f
  where f Nothing _ = False
        f (Just c) Nothing = isAlphaNum c
        f (Just c) (Just d) = isAlphaNum c && (not $ isAlphaNum d)

-- True if regular expression matches the empty String
empty :: RegMX a -> Bool
empty EpsMX = True
empty (SymMX _ _) = False
empty (PreMX _ s) = empty s
empty (PostMX s _ _) = empty s
empty (AltMX a b) = empty a || empty b
empty (SeqMX a b) = empty a && empty b
empty (RepMX _) = True

-- | True if final character of regular expression contains is matched
final :: RegMX a -- ^ regular expression
      -> Bool
final EpsMX = False
final (SymMX m _) = m
final (PreMX _ r) = final r
final (PostMX r b _) = b && final r
final (AltMX a b) = final a || final b
final (SeqMX a b) = (final a && empty b) || final b
final (RepMX a) = final a

shift :: Eq a => Bool -> RegMX a -> (Maybe a, a, Maybe a) -> RegMX a
shift _ EpsMX _ = EpsMX
shift m (SymMX _ c) (_, a, _) = SymMX (m && a == c) c -- previous symbol was marked and this one matches
shift m (PreMX f r) x@(mp, a, _) = PreMX f $ shift (m && f mp (Just a)) r x
shift m (PostMX r _ f) x@(_, a, mp) = PostMX (shift m r x) (f (Just a) mp) f
shift m (AltMX r s) x = AltMX (shift m r x) (shift m s x)
shift m (SeqMX r s) x = SeqMX (shift m r x)
                            $ shift (-- had a match & 1st part of SeqMX matches empty input
                                     (m && empty r)
                                     -- or first part has been matched up to the end
                                     || final r)
                                    s
                                    x
shift m (RepMX r) x = RepMX $ shift (m || final r) r x

zipDelayed :: a -> a -> [a] -> [(Maybe a, a, Maybe a)]
zipDelayed a b cs = merge (Just a:Just b:justified) (b:cs) justified
  where justified = map Just cs
        merge (x:_) (y:_) [] = [(x, y, Nothing)]
        merge (x:xs) (y:ys) (z:zs) = (x, y, z):merge xs ys zs
        merge [] _ _ = error "NOTREACHED 1"
        merge (_:_) [] _ = error "NOTREACHED 2"

matchMX :: (Show a, Eq a) => RegMX a -> [a] -> Bool
matchMX (PreMX f rx) [] = f Nothing Nothing && empty rx
matchMX (PostMX rx _ f) [] = f Nothing Nothing && empty rx
matchMX rx [] = empty rx
matchMX rx [a] = final $ shift True rx (Nothing, a, Nothing)
matchMX rx (a:b:bs) = final $ foldl' (shift False) (shift True rx (Nothing, a, Just b)) $ zipDelayed a b bs

-- emptyS: True if regular expression matches the empty String
-- finalS: True if final character of regular expression is matched, i.e.
--         if regular expression accepts the empty string as the end of
--         the match
data Reg s a = Reg { emptyS :: !s, finalS :: !s, regS :: RegS s a }

data RegS s a = EpsS
              | SymS (a -> s)
              | PreS (Maybe a -> Maybe a -> s) (Reg s a)
              | PostS (Reg s a) (Maybe a -> Maybe a -> s)
              | AltS (Reg s a) (Reg s a)
              | SeqS (Reg s a) (Reg s a)
              | RepS (Reg s a)

epsS :: Semiring s => Reg s a
epsS = Reg { emptyS=one, finalS=zero, regS=EpsS }

symS :: Semiring s => (a -> s) -> Reg s a
symS f = Reg { emptyS=zero, finalS=zero, regS=SymS f }

preS :: Semiring s => (Maybe a -> Maybe a -> s) -> Reg s a -> Reg s a
preS f r = r { regS = PreS f r }

postSgen :: Semiring s => Reg s a -> (Maybe a -> Maybe a -> s) -> Maybe a -> Maybe a -> Reg s a
postSgen r f x p = r { finalS = finalS r `stimes` f x p
                     , regS = PostS r f
                     }

postS :: Semiring s => Reg s a -> (Maybe a -> Maybe a -> s) -> Reg s a
postS r f = postSgen r f Nothing Nothing

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

shiftS :: Semiring s => s -> RegS s a -> (Maybe a, a, Maybe a) -> Reg s a
shiftS _ EpsS _ = epsS
shiftS m (SymS f) (_, x, _) = (symS f) { finalS=m `stimes` f x }
shiftS m (PreS f r) x@(mp, a, _) = preS f $ shiftS (m `stimes` f mp (Just a)) (regS r) x
shiftS m (PostS r f) x@(_, a, mb) = postSgen (shiftS m (regS r) x) f (Just a) mb
shiftS m (AltS p q) x = altS p' q'
  where p' = shiftS m (regS p) x
        q' = shiftS m (regS q) x
shiftS m (SeqS p q) x = seqS p' q'
  where p' = shiftS m (regS p) x
        q' = shiftS ((m `stimes` emptyS p) `splus` finalS p) (regS q) x
shiftS m (RepS r) x = repS $ shiftS (m `splus` finalS r) (regS r) x

matchS :: Semiring s => Reg s a -> [a] -> s
{-# SPECIALISE INLINE matchS :: Reg Bool Char -> String -> Bool #-}
matchS r [] = matchEmptyInput r
  where matchEmptyInput (Reg { regS=PreS f (Reg { emptyS=e })}) = e `stimes` f Nothing Nothing
        matchEmptyInput (Reg { regS=PostS (Reg { emptyS=e }) f}) = e `stimes` f Nothing Nothing
        matchEmptyInput (Reg { emptyS=e }) = e
matchS r [a] = finalS $ shiftS one (regS r) (Nothing, a, Nothing)
matchS r (a:b:bs) = finalS $ foldl' (shiftS zero . regS) (shiftS one (regS r) (Nothing, a, Just b)) $ zipDelayed a b bs

boolToSemiring :: Semiring s => Bool -> s
{-# SPECIALISE INLINE boolToSemiring :: Bool -> Bool #-}
boolToSemiring False = zero
boolToSemiring True = one

mxToS :: (Semiring s, Eq a) => RegMX a -> Reg s a
mxToS EpsMX = epsS
mxToS (SymMX _ a) = symS $ \x -> if x == a then one else zero
mxToS (PreMX f r) = preS (\x y -> boolToSemiring $ f x y) $ mxToS r
mxToS (PostMX r _ f) = postS (mxToS r) (\x y -> boolToSemiring $ f x y)
mxToS (AltMX a b) = altS (mxToS a) (mxToS b)
mxToS (SeqMX a b) = seqS (mxToS a) (mxToS b)
mxToS (RepMX r) = repS $ mxToS r

submatchW :: Semiring s => Reg s (Int, c) -> [c] -> s
submatchW r = matchS (seqS arb $ seqS r arb) . zip [0..]
  where arb = repS $ symS $ const one

unAnchor :: Semiring s => Reg s a -> Reg s a
unAnchor r = whatever `seqS` r `seqS` whatever
  where whatever = repS $ symS $ const one

tmatchS :: Semiring s => Reg s Char -> T.Text -> s
{-# SPECIALISE INLINE tmatchS :: Reg Bool Char -> T.Text -> Bool #-}
tmatchS r t
  | T.null t = matchEmptyInput r
  | otherwise = let a = T.head t
                    as = T.tail t in
                  if T.null as
                  then finalS $ shiftS one (regS r) (Nothing, a, Nothing)
                  else let b = T.head as
                           bs = T.tail as in
                         finalS (foldl' (shiftS zero . regS) (shiftS one (regS r) (Nothing, a, Just b)) $ zipDelayed a b $ T.unpack bs)
  where matchEmptyInput (Reg { regS=PreS f (Reg { emptyS=e })}) = e `stimes` f Nothing Nothing
        matchEmptyInput (Reg { regS=PostS (Reg { emptyS=e }) f}) = e `stimes` f Nothing Nothing
        matchEmptyInput (Reg { emptyS=e }) = e

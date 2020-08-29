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
             -- previous ---v---------v          v--- elt
             | PreMX (Maybe a) (Maybe a -> Maybe a -> Bool) (RegMX a)
             --       last seen elt ---v---------v          v--- peek next elt
             | PostMX (RegMX a) (Maybe a) (Maybe a -> Maybe a -> Bool)
             | AltMX (RegMX a) (RegMX a)
             | SeqMX (RegMX a) (RegMX a)
             | RepMX (RegMX a)

instance Show a => Show (RegMX a) where
  show EpsMX = "EpsMX"
  show (SymMX f a) = "(SymMX " ++ show f ++ " " ++ show a ++ ")"
  show (PreMX p _ r) = "(PreMX " ++ show p ++ " ? " ++ show r ++ ")"
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
startOfWordThen = PreMX Nothing f
  where f Nothing _ = True
        f (Just b) (Just c) = (not $ isAlphaNum b) && isAlphaNum c
        f (Just _) Nothing = False

endOfWordAfter :: RegMX Char -> RegMX Char
endOfWordAfter r = PostMX r Nothing f
  where f Nothing Nothing = True
        f (Just c) Nothing = isAlphaNum c
        f (Just c) (Just d) = isAlphaNum c && (not $ isAlphaNum d)
        f Nothing (Just _) = False

-- True if regular expression matches the empty String
empty :: RegMX a -> Bool
empty EpsMX = True
empty (SymMX _ _) = False
empty (PreMX _ _ s) = empty s
empty (PostMX s _ _) = empty s
empty (AltMX a b) = empty a || empty b
empty (SeqMX a b) = empty a && empty b
empty (RepMX _) = True

-- | True if final character of regular expression contains is matched
final :: RegMX a -- ^ regular expression
      -> Maybe a -- ^ peek next element
      -> Bool
final EpsMX _ = False
final (SymMX m _) _ = m
final (PreMX _ _ r) mb = final r mb
final (PostMX r ma f) mb = final r mb && f ma mb
final (AltMX a b) mb = final a mb || final b mb
final (SeqMX a b) mb = (final a mb && empty b) || final b mb
final (RepMX a) mb = final a mb

shift :: Eq a => Bool -> RegMX a -> a -> Maybe a -> RegMX a
shift _ EpsMX _ _ = EpsMX
shift m (SymMX _ c) a _ = SymMX (m && a == c) c -- previous symbol was marked and this one matches
shift m (PreMX p f r) a mb = PreMX (Just a) f $ shift (m && f p (Just a)) r a mb
shift m (PostMX r _ f) a mb = PostMX (shift m r a mb) (Just a) f
shift m (AltMX r s) a mb = AltMX (shift m r a mb) (shift m s a mb)
shift m (SeqMX r s) a mb = SeqMX (shift m r a mb)
                              $ shift (-- had a match & 1st part of SeqMX matches empty input
                                       (m && empty r)
                                       -- or first part has been matched up to the end
                                       || final r (Just a))
                                      s
                                      a
                                      mb
shift m (RepMX r) a mb = RepMX $ shift (m || final r (Just a)) r a mb

matchMX :: (Show a, Eq a) => RegMX a -> [a] -> Bool
matchMX (PreMX _ f rx) [] = f Nothing Nothing && empty rx
matchMX (PostMX rx _ f) [] = f Nothing Nothing && empty rx
matchMX rx [] = empty rx
matchMX rx [a] = final (shift True rx a Nothing) Nothing
matchMX rx (a:b:bs) = final (foldl' (shift_ False) (shift_ True rx (a, Just b)) $ zipped b bs) Nothing
  where shift_ f r (x, mb) = shift f r x mb
        zipped x [] = [(x, Nothing)]
        zipped x (y:ys) = (x, Just y):zipped y ys

-- emptyS: True if regular expression matches the empty String
-- finalS: True if final character of regular expression is matched, i.e.
--         if regular expression accepts the empty string as the end of
--         the match
data Reg s a = Reg { emptyS :: !s, finalS :: !s, regS :: RegS s a }

data RegS s a = EpsS
              | SymS (a -> s)
              | PreS (Maybe a) (Maybe a -> Maybe a -> s) (Reg s a)
              | PostS (Reg s a) (Maybe a -> Maybe a -> s)
              | AltS (Reg s a) (Reg s a)
              | SeqS (Reg s a) (Reg s a)
              | RepS (Reg s a)

epsS :: Semiring s => Reg s a
epsS = Reg { emptyS=one, finalS=zero, regS=EpsS }

symS :: Semiring s => (a -> s) -> Reg s a
symS f = Reg { emptyS=zero, finalS=zero, regS=SymS f }

preS :: Semiring s => Maybe a -> (Maybe a -> Maybe a -> s) -> Reg s a -> Reg s a
preS p f r = Reg { emptyS = emptyS r
                 , finalS = finalS r
                 , regS = PreS p f r
                 }
postS :: Semiring s => Reg s a -> (Maybe a -> Maybe a -> s) -> Maybe a -> Maybe a -> Reg s a
postS r f x p = r { finalS = finalS r `stimes` f x p
                  , regS = PostS r f
                  }

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

shiftS :: Semiring s => s -> RegS s a -> a -> Maybe a -> Reg s a
shiftS _ EpsS _ _ = epsS
shiftS m (SymS f) x _ = (symS f) { finalS=m `stimes` f x }
shiftS m (PreS ma f r) x mb = preS (Just x) f $ shiftS (m `stimes` f ma (Just x)) (regS r) x mb
shiftS m (PostS r f) x mb = postS (shiftS m (regS r) x mb) f (Just x) mb
shiftS m (AltS p q) x mb = altS p' q'
  where p' = shiftS m (regS p) x mb
        q' = shiftS m (regS q) x mb
shiftS m (SeqS p q) x mb = seqS p' q'
  where p' = shiftS m (regS p) x mb
        q' = shiftS ((m `stimes` emptyS p) `splus` finalS p) (regS q) x mb
shiftS m (RepS r) x mb = repS $ shiftS (m `splus` finalS r) (regS r) x mb

matchS :: Semiring s => Reg s a -> [a] -> s
{-# SPECIALISE INLINE matchS :: Reg Bool Char -> String -> Bool #-}
matchS r [] = matchEmptyInput r
  where matchEmptyInput (Reg { regS=PreS _ f (Reg { emptyS=e })}) = f Nothing Nothing `stimes` e
        matchEmptyInput (Reg { regS=PostS (Reg { emptyS=e }) f}) = f Nothing Nothing `stimes` e
        matchEmptyInput (Reg { emptyS=e }) = e
matchS r [a] = finalS $ shiftS one (regS r) a Nothing
matchS r (a:b:bs) = finalS (foldl' (shift_ zero . regS) (shift_ one (regS r) (a, Just b)) $ zipped b bs)
  where shift_ f rx (x, mb) = shiftS f rx x mb
        zipped x [] = [(x, Nothing)]
        zipped x (y:ys) = (x, Just y):zipped y ys

boolToSemiring :: Semiring s => Bool -> s
{-# SPECIALISE INLINE boolToSemiring :: Bool -> Bool #-}
boolToSemiring False = zero
boolToSemiring True = one

mxToS :: (Semiring s, Eq a) => RegMX a -> Reg s a
mxToS EpsMX = epsS
mxToS (SymMX _ a) = symS $ \x -> if x == a then one else zero
mxToS (PreMX p f r) = preS p (\x y -> boolToSemiring $ f x y) $ mxToS r
mxToS (PostMX r p f) = postS (mxToS r) (\x y -> boolToSemiring $ f x y) p Nothing
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
                  then finalS $ shiftS one (regS r) a Nothing
                  else let b = T.head as
                           bs = T.tail as in
                         finalS (foldl' (shift_ zero . regS) (shift_ one (regS r) (a, Just b)) $ zipped b $ T.unpack bs)
  where shift_ f rx (x, mb) = shiftS f rx x mb
        zipped x [] = [(x, Nothing)]
        zipped x (y:ys) = (x, Just y):zipped y ys
        matchEmptyInput (Reg { regS=PreS _ f (Reg { emptyS=e })}) = f Nothing Nothing `stimes` e
        matchEmptyInput (Reg { regS=PostS (Reg { emptyS=e }) f}) = f Nothing Nothing `stimes` e
        matchEmptyInput (Reg { emptyS=e }) = e

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
import Semiring (Semiring(..), SemiringEq(..))

data RegMX a = EpsMX
             | SymMX Bool a
             --      previous ---v    v--- elt
             | PreMX Bool (Maybe a -> a -> Bool) (RegMX a)
             --       last seen elt --v          v--- peek next elt
             | PostMX (RegMX a) Bool (a -> Maybe a -> Bool) Bool
             | AltMX (RegMX a) (RegMX a)
             | SeqMX (RegMX a) (RegMX a)
             | RepMX (RegMX a)

instance Show a => Show (RegMX a) where
  show EpsMX = "EpsMX"
  show (SymMX f a) = "(SymMX " ++ show f ++ " " ++ show a ++ ")"
  show (PreMX e _ r) = "(PreMX " ++ show e ++ " ? " ++ show r ++ ")"
  show (PostMX r p _ e) = "(PostMX " ++ show r ++ " " ++ show p ++ " ? " ++ show e ++ ")"
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
startOfWordThen = PreMX False f
  where f Nothing c = isAlphaNum c
        f (Just b) c = (not $ isAlphaNum b) && isAlphaNum c

endOfWordAfter :: RegMX Char -> RegMX Char
endOfWordAfter r = PostMX r False f False
  where f c Nothing = isAlphaNum c
        f c (Just d) = isAlphaNum c && (not $ isAlphaNum d)

-- True if regular expression matches the empty String
empty :: RegMX a -> Bool
empty EpsMX = True
empty (SymMX _ _) = False
empty (PreMX _ _ s) = empty s
empty (PostMX s _ _ _) = empty s
empty (AltMX a b) = empty a || empty b
empty (SeqMX a b) = empty a && empty b
empty (RepMX _) = True

-- | True if final character of regular expression contains is matched
final :: RegMX a -- ^ regular expression
      -> Bool
final EpsMX = False
final (SymMX m _) = m
final (PreMX _ _ r) = final r
final (PostMX r b _ _) = b && final r
final (AltMX a b) = final a || final b
final (SeqMX a b) = (final a && empty b) || final b
final (RepMX a) = final a

shift :: Eq a => Bool -> RegMX a -> (Maybe a, a, Maybe a) -> RegMX a
shift _ EpsMX _ = EpsMX
shift m (SymMX _ c) (_, a, _) = SymMX (m && a == c) c -- previous symbol was marked and this one matches
shift m (PreMX e f r) x@(mp, a, _) = PreMX e f $ shift (m && f mp a) r x
shift m (PostMX r _ f e) x@(_, a, mp) = PostMX (shift m r x) (f a mp) f e
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
matchMX (PreMX e _ rx) [] = e && empty rx
matchMX (PostMX rx _ _ e) [] = e && empty rx
matchMX rx [] = empty rx
matchMX rx [a] = final $ shift True rx (Nothing, a, Nothing)
matchMX rx (a:b:bs) = final $ foldl' (shift False) (shift True rx (Nothing, a, Just b)) $ zipDelayed a b bs

-- emptyS: True if regular expression matches the empty String
-- finalS: True if final character of regular expression is matched, i.e.
--         if regular expression accepts the empty string as the end of
--         the match
data Reg s a = Reg { activeS :: !Bool, emptyS :: !s, finalS :: !s, regS :: RegS s a }

data RegS s a = EpsS
              | SymS (a -> s)
              | PreS s (Maybe a -> a -> s) (Reg s a)
              | PostS (Reg s a) (a -> Maybe a -> s) s
              | AltS (Reg s a) (Reg s a)
              | SeqS (Reg s a) (Reg s a)
              | RepS (Reg s a)

epsS :: Semiring s => Reg s a
epsS = epsGen False

epsGen :: Semiring s => Bool -> Reg s a
epsGen a = Reg { activeS=a, emptyS=one, finalS=zero, regS=EpsS }

symS :: Semiring s => (a -> s) -> Reg s a
symS f = symSAF f False zero

symSAF :: Semiring s => (a -> s) -> Bool -> s -> Reg s a
symSAF f ac fi = Reg { activeS=ac, emptyS=zero, finalS=fi, regS=SymS f }

preS :: Semiring s => s -> (Maybe a -> a -> s) -> Reg s a -> Reg s a
preS e f r = r { regS = PreS e f r }

postSgen :: Semiring s => Reg s a -> (a -> Maybe a -> s) -> a -> Maybe a -> s -> Reg s a
postSgen r f x p e = r { finalS = finalA r `stimes` f x p
                       , regS = PostS r f e
                       }

postS :: Semiring s => Reg s a -> (a -> Maybe a -> s) -> s -> Reg s a
postS r f e = r { finalS = zero
                , regS = PostS r f e
                }

altS :: Semiring s => Reg s a -> Reg s a -> Reg s a
altS = altGen False

altGen :: Semiring s => Bool -> Reg s a -> Reg s a -> Reg s a
altGen a r s = Reg { emptyS = emptyS r `splus` emptyS s
                   , finalS = finalA r `splus` finalA s
                   , activeS = a
                   , regS = AltS r s
                   }

seqS :: Semiring s => Reg s a -> Reg s a -> Reg s a
seqS = seqGen False

seqGen :: Semiring s => Bool -> Reg s a -> Reg s a -> Reg s a
seqGen a r s = Reg { emptyS = emptyS r `stimes` emptyS s
                   , finalS = (finalA r `stimes` emptyS s) `splus` finalA s
                   , activeS = a
                   , regS = SeqS r s
                   }

repS :: Semiring s => Reg s a -> Reg s a
repS = repGen False

repGen :: Semiring s => Bool -> Reg s a -> Reg s a
repGen a r@(Reg { finalS = f }) = Reg { activeS = a, emptyS = one, finalS = f, regS = RepS r }

finalA :: Semiring s => Reg s a -> s
{-# SPECIALISE INLINE finalA :: Reg Bool Char -> Bool #-}
finalA r = if activeS r then finalS r else zero

shiftS :: SemiringEq s => s -> Reg s a -> (Maybe a, a, Maybe a) -> Reg s a
{-# SPECIALISE INLINE shiftS :: Bool -> Reg Bool Char -> (Maybe Char, Char, Maybe Char) -> Reg Bool Char #-}
shiftS m i@(Reg { activeS=a, regS=r }) x
  | isSZero m && not a = i -- NOP: no mark to shift in & current regexp is not active either
  | otherwise = stepS m r x

stepS :: SemiringEq s => s -> RegS s a -> (Maybe a, a, Maybe a) -> Reg s a
{-# SPECIALISE INLINE stepS :: Bool -> RegS Bool Char -> (Maybe Char, Char, Maybe Char) -> Reg Bool Char #-}
stepS _ EpsS _ = epsS
stepS m (SymS f) (_, x, _) = let end = m `stimes` f x
                                 active = not $ isSZero end in
                               symSAF f active end
stepS m (PreS e f r) x@(mp, a, _) = preS e f $ shiftS (m `stimes` f mp a) r x
stepS m (PostS r f e) x@(_, a, mb) = postSgen (shiftS m r x) f a mb e
stepS m (AltS p q) x = altGen (a || b) p' q'
  where p'@(Reg { activeS=a }) = shiftS m p x
        q'@(Reg { activeS=b }) = shiftS m q x
stepS m (SeqS p q) x = seqGen (a || b) p' q'
  where p'@(Reg { activeS=a }) = shiftS m p x
        q'@(Reg { activeS=b }) = shiftS ((m `stimes` emptyS p) `splus` finalA p) q x
stepS m (RepS r) x = let r'@(Reg { activeS=a }) = shiftS (m `splus` finalA r) r x in
                       repGen a r'

matchS :: SemiringEq s => Reg s a -> [a] -> s
{-# SPECIALISE INLINE matchS :: Reg Bool Char -> String -> Bool #-}
matchS r [] = matchEmptyInput r
  where matchEmptyInput (Reg { regS=PreS v _ (Reg { emptyS=e })}) = e `stimes` v
        matchEmptyInput (Reg { regS=PostS (Reg { emptyS=e }) _ v}) = e `stimes` v
        matchEmptyInput (Reg { emptyS=e }) = e
matchS r [a] = finalS $ shiftS one r (Nothing, a, Nothing)
matchS r (a:b:bs) = finalS $ foldl' (shiftS zero) (shiftS one r (Nothing, a, Just b)) $ zipDelayed a b bs

boolToSemiring :: Semiring s => Bool -> s
{-# SPECIALISE INLINE boolToSemiring :: Bool -> Bool #-}
boolToSemiring False = zero
boolToSemiring True = one

mxToS :: (Semiring s, Eq a) => RegMX a -> Reg s a
mxToS EpsMX = epsS
mxToS (SymMX _ a) = symS $ \x -> if x == a then one else zero
mxToS (PreMX e f r) = preS (boolToSemiring e) (\x y -> boolToSemiring $ f x y) $ mxToS r
mxToS (PostMX r _ f e) = postS (mxToS r) (\x y -> boolToSemiring $ f x y) (boolToSemiring e)
mxToS (AltMX a b) = altS (mxToS a) (mxToS b)
mxToS (SeqMX a b) = seqS (mxToS a) (mxToS b)
mxToS (RepMX r) = repS $ mxToS r

submatchW :: SemiringEq s => Reg s (Int, c) -> [c] -> s
submatchW r = matchS (seqS arb $ seqS r arb) . zip [0..]
  where arb = repS $ symS $ const one

unAnchor :: Semiring s => Reg s a -> Reg s a
unAnchor r = whatever `seqS` r `seqS` whatever
  where whatever = repS $ symS $ const one

tmatchS :: SemiringEq s => Reg s Char -> T.Text -> s
{-# SPECIALISE INLINE tmatchS :: Reg Bool Char -> T.Text -> Bool #-}
tmatchS r t
  | T.null t = matchEmptyInput r
  | otherwise = let a = T.head t
                    as = T.tail t in
                  if T.null as
                  then finalS $ shiftS one r (Nothing, a, Nothing)
                  else let b = T.head as
                           bs = T.tail as in
                         finalS (foldl' (shiftS zero) (shiftS one r (Nothing, a, Just b)) $ zipDelayed a b $ T.unpack bs)
  where matchEmptyInput (Reg { regS=PreS v _ (Reg { emptyS=e })}) = e `stimes` v
        matchEmptyInput (Reg { regS=PostS (Reg { emptyS=e }) _ v}) = e `stimes` v
        matchEmptyInput (Reg { emptyS=e }) = e

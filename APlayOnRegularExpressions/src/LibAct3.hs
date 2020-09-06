module LibAct3 (
  LeftLong(..)
  , LeftmostMatch(..)
  , Range(..)
  , Reg
  , SemiringI(..)
  , Start(..)
  , altS
  , epsS
  , matchS
  , mxToS
  , repS
  , seqS
  , shiftS
  , symS
  , tmatchS
  , unAnchor
)
where

import Data.List (foldl')
import qualified Data.Text as T
import Semiring (Semiring(..), SemiringEq(..))

import LibAct2 (
  LeftLong(..)
  , LeftmostMatch(..)
  , Range(..)
  , RegMX(..)
  , SemiringI(..)
  , Start(..)
  )

-- active: True if weight of regular expression is not zero.  The exported
-- smart constructors initialize it to False without looking inside to allow
-- infinite/recursive definitions of regular expressions (context-free
-- grammar)
--
-- emptyS: one if regular expression matches the empty String
--
-- finalS: one if final character of regular expression is matched, i.e.
--         if regular expression accepts the empty string as the end of
--         the match
data Reg s a = Reg { activeS :: !Bool, emptyS :: !s, finalS :: !s, regS :: RegS s a }

data RegS s a = EpsS
              | SymS (a -> s)
              | AltS (Reg s a) (Reg s a)
              | SeqS (Reg s a) (Reg s a)
              | RepS (Reg s a)

epsS :: Semiring s => Reg s a
epsS = Reg { activeS=False, emptyS=one, finalS=zero, regS=EpsS }

symSAF :: Semiring s => Bool -> s -> (a -> s) -> Reg s a
symSAF hasMark final f = Reg { activeS=hasMark, emptyS=zero, finalS=final, regS=SymS f }

symS :: Semiring s => (a -> s) -> Reg s a
symS = symSAF False zero

altS, altS_ :: Semiring s => Reg s a -> Reg s a -> Reg s a
altS r s = Reg { activeS = False
               , emptyS = emptyS r `splus` emptyS s
               , finalS = zero
               , regS = AltS r s
               }

-- Alternative version of the exported smart constructor used for the
-- evaluation of the regular expression: it propagates the value of activeS.
altS_ r s = Reg { activeS = activeS r || activeS s
                , emptyS = emptyS r `splus` emptyS s
                , finalS = finalA r `splus` finalA s
                , regS = AltS r s
                }

seqS, seqS_ :: Semiring s => Reg s a -> Reg s a -> Reg s a
seqS r s = Reg { activeS = False
               , emptyS = emptyS r `stimes` emptyS s
               , finalS = zero
               , regS = SeqS r s
               }

-- Alternative version of the exported smart constructor used for the
-- evaluation of the regular expression: it propagates the value of activeS.
seqS_ r s = Reg { activeS = activeS r || activeS s
                , emptyS = emptyS r `stimes` emptyS s
                , finalS = (finalA r `stimes` emptyS s) `splus` finalA s
                , regS = SeqS r s
                }

repS, repS_ :: Semiring s => Reg s a -> Reg s a
repS r = Reg { activeS = False
             , emptyS = one
             , finalS = zero
             , regS = RepS r }

-- Alternative version of the exported smart constructor used for the
-- evaluation of the regular expression: it propagates the value of activeS.
repS_ r = Reg { activeS = activeS r
              , emptyS = one
              , finalS = finalA r
              , regS = RepS r }

finalA :: Semiring s => Reg s a -> s
{-# SPECIALISE INLINE finalA :: Reg Bool Char -> Bool #-}
finalA r = if activeS r then finalS r else zero

shiftS :: SemiringEq s => s -> Reg s a -> a -> Reg s a
{-# SPECIALISE INLINE shiftS :: Bool -> Reg Bool Char -> Char -> Reg Bool Char #-}
shiftS m (Reg { regS=SymS f }) x = let final = m `stimes` f x
                                       active = not $ isSZero final in
                                     symSAF active final f
shiftS _ r@(Reg { regS=EpsS }) _ = r
shiftS m r a | (not $ activeS r) && isSZero m = r
             | otherwise = stepS m (regS r) a

stepS :: SemiringEq s => s -> RegS s a -> a -> Reg s a
{-# SPECIALISE INLINE stepS :: Bool -> RegS Bool Char -> Char -> Reg Bool Char #-}
stepS m (AltS p q) x = altS_ p' q'
  where p' = shiftS m p x
        q' = shiftS m q x
stepS m (SeqS p q) x = seqS_ p' q'
  where p' = shiftS m p x
        q' = shiftS (finalA p `splus` (m `stimes` emptyS p)) q x
stepS m (RepS r) x = repS_ $ shiftS (m `splus` finalA r) r x
stepS _ _ _ = error "This case should have been handled in shiftS"

matchS :: SemiringEq s => Reg s a -> [a] -> s
{-# SPECIALISE INLINE matchS :: Reg Bool Char -> String -> Bool #-}
matchS r [] = emptyS r
matchS r (a:as) = finalA $ foldl' (shiftS zero) (shiftS one r a) as

mxToS :: (Semiring s, Eq a) => RegMX a -> Reg s a
mxToS EpsMX = epsS
mxToS (SymMX _ a) = symS $ \x -> if x == a then one else zero
mxToS (AltMX a b) = altS (mxToS a) (mxToS b)
mxToS (SeqMX a b) = seqS (mxToS a) (mxToS b)
mxToS (RepMX r) = repS $ mxToS r

unAnchor :: Semiring s => Reg s a -> Reg s a
unAnchor r = whatever `seqS` r `seqS` whatever
  where whatever = repS $ symS $ const one

tmatchS :: SemiringEq s => Reg s Char -> T.Text -> s
{-# SPECIALISE INLINE tmatchS :: Reg Bool Char -> T.Text -> Bool #-}
tmatchS r t
  | T.null t = emptyS r
  | otherwise = let a = T.head t
                    as = T.tail t in
                  finalA $ T.foldl' (shiftS zero) (shiftS one r a) as

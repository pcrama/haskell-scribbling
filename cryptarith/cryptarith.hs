#!/usr/bin/env runghc

import Data.List (nub)

data Solution = S { _a ::Int, _e :: Int, _i :: Int, _m :: Int, _o :: Int, _t :: Int }
  deriving (Show, Eq)

xyz :: (a -> Int) -> (a -> Int) -> (a -> Int) -> a -> Int
xyz x y z s = a * 100 + b * 10 + c
  where a = x s
        b = y s
        c = z s

toi :: Solution -> Int
toi = xyz _t _o _i

et :: Solution -> Int
et = xyz (const 0) _e _t

moi :: Solution -> Int
moi = xyz _m _o _i

toi_et_moi :: Solution -> Int
toi_et_moi = pure (+) <*> toi <*> (pure (+) <*> et <*> moi)

emoi :: Solution -> Int
emoi = pure (+) <*> ((*1000) <$> _e) <*> moi

aime :: Solution -> Int
aime = pure (+) <*> ((*1000) <$> _a) <*> xyz _i _m _e

team :: Solution -> Int
team = pure (+) <*> ((*1000) <$> _t) <*> xyz _e _a _m

mate :: Solution -> Int
mate = pure (+) <*> ((*1000) <$> _m) <*> xyz _a _t _e

-- 2 solutions if a /= 0, e <= i <= m <= o <= t
-- 1 solution  if a /= 0, e < i < m < o < t
-- S {_a = 1, _e = 1, _i = 2, _m = 4, _o = 6, _t = 7}
-- S {_a = 1, _e = 2, _i = 2, _m = 3, _o = 5, _t = 8}
--   T O I        762    852
--     E T         17     28
--   M O I        462    352
-- -------       1241   1232
-- A I M E

valid :: Solution -> Bool
valid s =
  (aime s == toi_et_moi s)
  && a /= 0
  && a <= e
  && e <= i
  && i <= m
  && m <= o
  && o <= t
  -- && e /= 0
  -- && i /= 0
  -- && m /= 0
  -- && o /= 0
  -- && t /= 0
  -- && (length $ nub [a, e, m, o, t]) > 4
  where a = _a s
        e = _e s
        i = _i s
        m = _m s
        o = _o s
        t = _t s

solutions :: [Solution]
solutions = do
  t <- [0..9]
  m <- [0..9]
  -- let i = 1
  i <- [0..9]
  e <- [0..9]
  o <- [0..9]
  a <- [0..9]
  let s = S { _a = a, _e = e, _i = i, _m = m, _o = o, _t = t } 
  if valid s then return s else []

main = mapM_ print $ take 5 solutions

-- Local Variables:
-- compile-command: "runghc cryptarith.hs"
-- End:

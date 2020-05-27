module Lib (
  RegX(..)
  , Reg
  , accept
  , nocs
  , onec
  , evencs
  , splits
  )
where

data RegX a = Eps
            | Sym a
            | Alt (RegX a) (RegX a)
            | Seq (RegX a) (RegX a)
            | Rep (RegX a)

type Reg = RegX Char

nocs = Rep $ Alt (Sym 'a') (Sym 'b')
onec = Seq nocs $ Sym 'c'
evencs = Seq (Rep (Seq onec onec)) nocs

accept :: Eq a => RegX a -> [a] -> Bool
accept Eps [] = True
accept Eps _ = False
accept (Sym x) [y] = x == y
accept (Sym _) _ = False
accept (Alt x y) s = accept x s || accept y s
accept (Seq x y) s = or [ accept x s1 && accept y s2
                        | (s1, s2) <- splits s ]
accept (Rep _) [] = True
accept rr@(Rep r) s = or [ accept r s1 && accept rr s2
                         | (s1, s2) <- splits s ]

splits :: [a] -> [([a], [a])]
splits [] = [([], [])]
splits [x] = [([x], []), ([], [x])]
splits (x:xs) = ([], x:xs):map (\(y, z) -> (x:y, z)) (splits xs)
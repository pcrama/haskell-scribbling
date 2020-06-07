module Lib (
  RegX(..)
  , Reg
  , accept
  , acceptA1S1
  , evencs
  , nocs
  , onec
  , parts
  , plus
  , sequ
  , splits
  )
where

data RegX a = Eps
            | Sym a
            | Alt (RegX a) (RegX a)
            | Seq (RegX a) (RegX a)
            | Rep (RegX a)

type Reg = RegX Char

nocs, onec, evencs :: Reg
nocs = Rep $ Alt (Sym 'a') (Sym 'b')
onec = Seq nocs $ Sym 'c'
evencs = Seq (Rep (Seq onec onec)) nocs

plus :: RegX a -> RegX a
plus Eps = Eps
plus r = Seq r (Rep r)

sequ :: [a] -> RegX a
sequ [] = Eps
sequ [x] = Sym x
sequ (x:xs) = Seq (Sym x) $ sequ xs

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
splits [x] = [([], [x]), ([x], [])]
splits (x:xs) = ([], x:xs):map (\(y, z) -> (x:y, z)) (splits xs)

-- implementation from pearl, Act 1, Scene 1
acceptA1S1 :: Eq a => RegX a -> [a] -> Bool
acceptA1S1 Eps       u = null u
acceptA1S1 (Sym x)   u = [x] == u
acceptA1S1 (Alt x y) u = acceptA1S1 x u || acceptA1S1 y u
acceptA1S1 (Seq x y) u = or [acceptA1S1 x v && acceptA1S1 y w
                            | (v, w) <- splits u]
acceptA1S1 (Rep r)   u = or [and [acceptA1S1 r v | v <- ps]
                            | ps <- parts u]

-- parts "cc" `shouldBe` [["cc"], ["c", "c"]]
-- parts "acc" `shouldBe` [["acc"], ["a", "cc"], ["ac", "c"], ["a", "c", "c"]]
parts :: [a] -> [[[a]]]
parts [] = [[]]
parts [c] = [[[c]]]
parts (c:cs) = concatMap (\(p:ps) -> [(c:p):ps, [c]:p:ps]) $ parts cs

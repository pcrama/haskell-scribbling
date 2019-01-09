{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

data ExprP0 a
  = Val0 Int
  | Add0 a a

instance Functor ExprP0 where
  fmap f (Val0 v) = Val0 v
  fmap f (Add0 a b) = Add0 (f a) (f b)

newtype Fix f = Fix { unFix :: f (Fix f) }

cata :: Functor p => (p a -> a) -> Fix p -> a
cata f = f . fmap (cata f) . unFix

type Expr0 = Fix ExprP0

eval0 :: Expr0 -> Int
eval0 = cata smallEval
  where smallEval (Val0 x) = x
        smallEval (Add0 x y) = x + y

pp0 :: Expr0 -> String
pp0 = cata smallPp
  where smallPp :: ExprP0 String -> String
        smallPp (Val0 v) = show v
        smallPp (Add0 x y) = "(" ++ x ++ "+" ++ y ++ ")"

data ListF a b
    = LFNil
    | LFCons a b

instance Functor (ListF a) where
  fmap _ LFNil = LFNil
  fmap f (LFCons a b) = LFCons a $ f b

type List a = Fix (ListF a)

lfoldr :: (a -> b -> b) -> b -> List a -> b
lfoldr f base = cata f'
  where f' LFNil = base
        f' (LFCons a b) = f a b

lnil = Fix LFNil
lcons hd tl = Fix $ LFCons hd tl

val0 = Fix . Val0
add0 x y = Fix $ Add0 x y

list2listf = foldr lcons lnil

newtype K1 a  x = K1 a                -- constant
newtype Id    x = Id x                -- element
data Sum1 p q x = L1 (p x) | R1 (q x) -- choice
data Prd1 p q x = Prd1 (p x) (q x)    -- pairing

type One1 = K1 ()

-- Maybe re-expressed with above types
type Mebbe x = Sum1 One1 Id x

mebbeNothing = L1 $ K1 ()
mebbeJust    = R1 . Id
mebbe       :: b -> (a -> b) -> Mebbe a -> b
mebbe b _ (L1 (K1 ())) = b
mebbe _ f (R1 (Id x)) = f x

instance Functor (K1 a) where
  fmap _ (K1 x) = K1 x

instance Functor Id where
  fmap f (Id x) = Id $ f x

instance (Functor p, Functor q) => Functor (Sum1 p q) where
  fmap f (L1 px) = L1 $ fmap f px
  fmap f (R1 qx) = R1 $ fmap f qx

instance (Functor p, Functor q) => Functor (Prd1 p q) where
  fmap f (Prd1 px qx) = Prd1 (fmap f px) (fmap f qx)

type ExprP1 = Sum1 (K1 Int) (Prd1 Id Id)
type Expr1 = Fix ExprP1

pattern Val1 v = L1 (K1 v)
pattern Add1 x y = R1 (Prd1 (Id x) (Id y))

val1 = Fix . L1 . K1
add1 x y = Fix . R1 $ Prd1 (Id x) (Id y)

eval1 :: Expr1 -> Int
eval1 = cata smallEval
  where smallEval (Val1 v) = v
        smallEval (Add1 x y) = x + y

pp1 :: Expr1 -> String
pp1 = cata $ \case
  Val1 v -> show v
  Add1 x y -> '(':x ++ "+" ++ y ++ ")"

newtype K2 a  x y = K2 a                    -- constant
newtype Fst   x y = Fst x                   -- element
newtype Snd   x y = Snd y                   -- element
data Sum2 p q x y = L2 (p x y) | R2 (q x y) -- choice
data Prd2 p q x y = Prd2 (p x y) (q x y)    -- pairing

type One2 = K2 ()

class Bifunctor p where
  dimap :: (a -> x) -> (b -> y) -> p a b -> p x y
  lmap :: (a -> x) -> p a y -> p x y
  lmap = flip dimap id
  rmap :: (b -> y) -> p x b -> p x y
  rmap = dimap id

instance Bifunctor (K2 a) where
  dimap _ _ (K2 x) = K2 x

instance Bifunctor Fst where
  dimap f _ (Fst x) = Fst $ f x

instance Bifunctor Snd where
  dimap _ f (Snd x) = Snd $ f x

instance (Bifunctor p, Bifunctor q) => Bifunctor (Sum2 p q) where
  dimap f g (L2 p) = L2 $ dimap f g p
  dimap f g (R2 q) = R2 $ dimap f g q

instance (Bifunctor p, Bifunctor q) => Bifunctor (Prd2 p q) where
  dimap f g (Prd2 p q) = Prd2 (dimap f g p) (dimap f g q)

data Zero                       -- has no values

magic :: Zero -> a
magic z = z `seq` error "Never get this far because no values exist for Zero"

inflate :: Functor p => p Zero -> p a
inflate = fmap magic            -- also unsafeCoerce#

type Zero1 = K1 Zero
type Zero2 = K2 Zero

newtype AllClowns p c j = AllClowns (p c) -- AllClowns Id === Fst
newtype AllJokers p c j = AllJokers (p j) -- AllJokers Id === Snd

instance Functor p => Bifunctor (AllClowns p) where
  dimap f _ (AllClowns pc) = AllClowns $ fmap f pc

instance Functor p => Bifunctor (AllJokers p) where
  dimap _ g (AllJokers pj) = AllJokers $ fmap g pj

class (Functor p, Bifunctor (DissectionContainer p)) => Diss p c j where
  type DissectionContainer p :: * -> * -> *
  -- either at    far left, ie. -or- in the middle (provide clown
  --              only jokers        to plug the hole)
  right :: Either (p j)             (DissectionContainer p c j, c)
        --        still in middle, return evicted joker and new
        --        dissection
        -> Either (j, DissectionContainer p c j)
        --  -or-  plugged last hole with clown, only clowns left
                  (p c)
  -- either at   far right, ie. -or- in the middle (provide joker
  --             only clowns         to plug the hole)
  left :: Either (p c)              (DissectionContainer p c j, j)
       --        still in middle, return evicted clown and new
       --        dissection
       -> Either (c, DissectionContainer p c j)
       --  -or-  plugged last hole with joker, only jokers left
                 (p j)

instance (Diss p c j, Diss q c j) => Diss (Prd1 p q) c j where
  type DissectionContainer (Prd1 p q) = Sum2 (Prd2 (DissectionContainer p) (AllJokers q))
                                             (Prd2 (AllClowns p) (DissectionContainer q))
  right (Left (Prd1 pj qj)) = case right $ Left pj of
    Left (j, pcj) -> Left (j, L2 $ Prd2 pcj (AllJokers qj))
    Right pc -> case right $ Left qj of
      Left (j, qcj) -> Left (j, R2 $ Prd2 (AllClowns pc) qcj)
      Right qc -> Right $ Prd1 pc qc
  right (Right (L2 (Prd2 pcj (AllJokers qj)), c)) = case right $ Right (pcj, c) of
    Left (j, pcj1) -> Left (j, L2 $ Prd2 pcj1 (AllJokers qj))
    Right (AllClowns pc) -> case right $ Left qj of
      Left (j, qcj) -> Left (j, R2 $ Prd2 (AllClowns pc) qcj)
      Right (AllClowns qc) -> Right $ Prd1 pc qc
  right (Right (R2 (Prd2 (AllClowns pc) qcj), c)) = case right $ Right (qcj, c) of
    Left (j, qcj1) -> Left (j, L2 $ Prd2 (AllClowns pc) qcj1)
    Right qc -> Right $ Prd1 pc qc
  right _ = undefined

instance (Diss p c j, Diss q c j) => Diss (Sum1 p q) c j where
  right (Left (L1 pj)) = case right $ Left pj of
    Left (j, pcj1) -> Left (j, L2 pcj1)
    Right pc -> Right $ L1 pc
  right (Left (R1 qj)) = case right $ Left qj of
    Left (j, qcj) -> Left (j, R2 qcj)
    Right qc -> Right $ R1 qc
  right (Right (L2 pcj, c)) = case right $ Right (pcj, c) of
    Left (j, pcj1) -> Left (j, L2 pcj1)
    Right pc -> Right $ L1 pc
  right (Right (R2 qcj, c)) = case right $ Right (qcj, c) of
    Left (j, qcj1) -> Left (j, R2 qcj1)
    Right pc -> Right $ R1 pc
  left (Left (L1 pc)) = case left $ Left pc of
    Left (c, pcj) -> Left (c, L2 pcj)
    Right pj -> Right $ L1 pj
  left (Left (R1 qc)) = case left $ Left qc of
    Left (c, qcj) -> Left (c, R2 qcj)
    Right qj -> Right $ R1 qj
  left (Right (L2 pcj, j)) = case left $ Right (pcj, j) of
    Left (c, pcj) -> Left (c, L2 pcj)
    Right pj -> Right $ L1 pj
  left (Right (R2 qcj, j)) = case left $ Right (qcj, j) of
    Left (c, qcj) -> Left (c, R2 qcj)
    Right qj -> Right $ R1 qj
  type DissectionContainer (Sum1 p q) = Sum2 (DissectionContainer p) (DissectionContainer q)

-- newtype Fst   x y = Fst x               -- element
-- newtype Snd   x y = Snd y               -- element
-- data Sum2 p q x y = L2 (p x) | R2 (q y) -- choice
-- data Prd2 p q x y = Prd2 (p x) (q y)    -- pairing
instance Diss (K1 a) c j where
  right (Left (K1 a)) = Right $ K1 a
  right (Right (K2 z, _)) = magic z
  left (Left (K1 a)) = Right $ K1 a
  left (Right (K2 z, _)) = magic z
  type DissectionContainer (K1 a) = K2 Zero

instance Diss Id c j where
  right (Left (Id j)) = Left (j, K2 ())
  right (Right (K2 (), c)) = Right $ Id c
  left (Left (Id c)) = Left (c, K2 ())
  left (Right (K2 (), j)) = Right $ Id j
  type DissectionContainer Id = One2

main = do
  putStrLn . show $ lfoldr (+) 0 $ list2listf [1..4]
  putStrLn $ lfoldr (\a b -> show a ++ b) "." $ list2listf [1..4]
  let exp val add = add (val 1) (add (add (val 2) (val 3)) (val 4))
  let exp0 = exp val0 add0
  putStrLn $ "0: " ++ pp0 exp0 ++ "=" ++ show (eval0 exp0)
  let exp1 = exp val1 add1
  putStrLn $ "1: " ++ pp1 exp1 ++ "=" ++ show (eval1 exp1)

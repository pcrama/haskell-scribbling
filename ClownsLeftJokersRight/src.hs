{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

data ExprP0 a
  = Val0 Int
  | Add0 a a

instance Functor ExprP0 where
  fmap _ (Val0 v) = Val0 v
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

listFsum :: List Int -> Int
listFsum = cata summer
  where summer :: ListF Int Int -> Int
        summer LFNil = 0
        summer (LFCons a b) = a + b

lfoldr :: (a -> b -> b) -> b -> List a -> b
lfoldr f base = cata f'
  where f' LFNil = base
        f' (LFCons a b) = f a b

lnil :: Fix (ListF a)
lnil = Fix LFNil

lcons :: a -> Fix (ListF a) -> Fix (ListF a)
lcons hd tl = Fix $ LFCons hd tl

val0 :: Int -> Fix ExprP0
val0 = Fix . Val0

add0 :: Fix ExprP0 -> Fix ExprP0 -> Fix ExprP0
add0 x y = Fix $ Add0 x y

list2listf :: [a] -> Fix (ListF a)
list2listf = foldr lcons lnil

newtype K1 a  x = K1 a                -- constant
newtype Id    x = Id x                -- element
data Sum1 p q x = L1 (p x) | R1 (q x) -- choice
data Prd1 p q x = Prd1 (p x) (q x)    -- pairing

type One1 = K1 ()

-- Maybe re-expressed with above types
type Mebbe x = Sum1 One1 Id x

mebbeNothing :: Sum1 (K1 ()) Id x
mebbeNothing = L1 $ K1 ()
mebbeJust :: x -> Sum1 (K1 ()) Id x
mebbeJust  = R1 . Id
mebbe :: b -> (a -> b) -> Mebbe a -> b
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

pattern Val1 :: Int -> ExprP1 t
pattern Val1 v = L1 (K1 v)

pattern Add1 :: t -> t -> ExprP1 t
pattern Add1 x y = R1 (Prd1 (Id x) (Id y))

val1 :: Int -> Expr1
val1 = Fix . L1 . K1

add1 :: Expr1 -> Expr1 -> Expr1
add1 x y = Fix . R1 $ Prd1 (Id x) (Id y)

-- tcata is cata re-expressed with tmap, made fully tail recursive
tcata :: Diss p a (Fix p) => (p a -> a) -> Fix p -> a
-- This would be cheating: it still isn't fully tail recursive (in tcata)
-- tcata f = f . tmap (tcata f) . unFix
--
-- Use `right' to replace Fix p (the jokers) by a (the clowns pushed
-- to the left).  Once all Fix p in p are replaced, we have a p a,
-- that can be made into an a by f.  The recursion is managed by
-- explicitly managing the stack.
--
-- `right', specialized for our case:
-- right :: Either (p (Fix p)) (DissectionContainer p a (Fix p), a)
--       -> Either (Fix p, DissectionContainer p a (Fix p)) (p a)
tcata f fpfp = load f fpfp []
  where load :: Diss p a (Fix p) => (p a -> a) -> Fix p -> [DissectionContainer p a (Fix p)] -> a
        load f (Fix pfp) = next f (right $ Left pfp)
        next :: Diss p a (Fix p)
             => (p a -> a)
             -> Either (Fix p, DissectionContainer p a (Fix p)) (p a)
             -> [DissectionContainer p a (Fix p)]
             -> a
        next f (Left (fpfp, pcj)) stk = load f fpfp $ pcj:stk
        next f (Right pa) stk = unload f pa stk
        unload :: Diss p a (Fix p)
               => (p a -> a)
               -> p a
               -> [DissectionContainer p a (Fix p)]
               -> a
        unload f pa [] = f pa
        unload f pa (pcj:stk) = next f (right $ Right (pcj, f pa)) stk

eval1 :: Expr1 -> Int
eval1 = tcata smallEval
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
  plug :: c ~ j => c -> DissectionContainer p c c -> p c

instance Diss (K1 a) c j where
  right (Left (K1 a)) = Right $ K1 a
  right (Right (K2 z, _)) = magic z
  left (Left (K1 a)) = Right $ K1 a
  left (Right (K2 z, _)) = magic z
  type DissectionContainer (K1 a) = K2 Zero
  plug _ (K2 z) = K1 $ magic z

instance Diss Id c j where
  right (Left (Id j)) = Left (j, K2 ())
  right (Right (K2 (), c)) = Right $ Id c
  left (Left (Id c)) = Left (c, K2 ())
  left (Right (K2 (), j)) = Right $ Id j
  type DissectionContainer Id = One2
  plug x _ = Id x

instance (Diss p c j, Diss q c j) => Diss (Sum1 p q) c j where
  right = \case
      Left (L1 pj) -> repackP $ Left pj
      Right (L2 pcj, c) -> repackP $ Right (pcj, c)
      Left (R1 qj) -> repackQ $ Left qj
      Right (R2 qcj, c) -> repackQ $ Right (qcj, c)
    where repackP = repackSumAsP right
          repackQ = repackSumAsQ right
  left = \case
      Left (L1 pc) -> repackP $ Left pc
      Right (L2 pcj, j) -> repackP $ Right (pcj, j)
      Left (R1 qc) -> repackQ $ Left qc
      Right (R2 qcj, j) -> repackQ $ Right (qcj, j)
    where repackP = repackSumAsP left
          repackQ = repackSumAsQ left
  type DissectionContainer (Sum1 p q) = Sum2 (DissectionContainer p) (DissectionContainer q)
  plug x (L2 pcj) = L1 $ plug x pcj
  plug x (R2 qcj) = R1 $ plug x qcj

-- Probably only useful as auxiliary function for the Diss (Sum p q) x
-- y instance, but I don't know how to make it more local.  The type
-- signature is made more specialized than needed on purpose to make
-- it clearer.  c1 can't be c & j1 can't be j because repackSumAsP
-- will be used with left and right.
repackSumAsP :: (Diss p c j, Diss q c j)
             => (Either (p t) (DissectionContainer p c j, z)
                 -> Either (j1, DissectionContainer p c j) (p c1))
             -> (Either (p t) (DissectionContainer p c j, z))
             -> Either (j1, Sum2 (DissectionContainer p)
                                (DissectionContainer q)
                                c
                                j)
                       (Sum1 p q c1)
repackSumAsP f x = case f x of
  Left (j, pcj) -> Left (j, L2 pcj)
  Right pc -> Right $ L1 pc

-- Probably only useful as auxiliary function for the Diss (Sum p q) x
-- y instance, but I don't know how to make it more local.  The type
-- signature is made more specialized than needed on purpose to make
-- it clearer.  c1 can't be c & j1 can't be j because repackSumAsP
-- will be used with left and right.
--
-- Most general type signature (proposed by ghc):
-- repackSumAsQ :: (a -> Either (t, q x y) (q1 x1))
--              -> a
--              -> Either (t, Sum2 p q x y) (Sum1 p1 q1 x1)
repackSumAsQ :: (Diss p c j, Diss q c j)
             => (Either (q t) (DissectionContainer q c j, z)
                 -> Either (j1, DissectionContainer q c j) (q c1))
             -> (Either (q t) (DissectionContainer q c j, z))
             -> Either (j1, Sum2 (DissectionContainer p)
                                (DissectionContainer q)
                                c
                                j)
                       (Sum1 p q c1)
repackSumAsQ f x = case f x of
  Left (j, qcj) -> Left (j, R2 qcj)
  Right qc -> Right $ R1 qc

instance (Diss p c j, Diss q c j) => Diss (Prd1 p q) c j where
  right = \case
      -- Only jokers in Prd1 -> start with first component of Prd1
      Left (Prd1 pj qj) -> dissect1stComponent qj $ Left pj
      -- Dissection of first component -> continue dissecting it
      Right (L2 (Prd2 pcj (AllJokers qj)), c) -> dissect1stComponent qj $ Right (pcj, c)
      -- Dissection of second component -> continue dissecting it
      Right (R2 (Prd2 (AllClowns pc) qcj), c) -> case right $ Right (qcj, c) of
        -- a Joker & continued dissection of qcj -> return Joker & repackage in Prd2
        Left (j, qcj1) -> Left (j, R2 $ Prd2 (AllClowns pc) qcj1)
        -- no Jokers left in qcj -> return Prd1 of Clowns only
        Right qc -> Right $ Prd1 pc qc
    where dissect1stComponent qj x = case right x of
            -- a Joker and dissection of pj -> return Joker & repackage in Prd2
            Left (j, pcj) -> Left (j, L2 $ Prd2 pcj (AllJokers qj))
            -- there were no jokers in pj -> look for one in second component of Prd1
            Right pc -> case right $ Left qj of
              -- a Joker & dissection of qj -> return Joker & repackage in Prd2
              Left (j, qcj) -> Left (j, R2 $ Prd2 (AllClowns pc) qcj)
              -- there were no Jokers in qj -> return Prd1 of Clowns only
              Right qc -> Right $ Prd1 pc qc
  left = goLeft
    where goLeft (Left (Prd1 pc qc)) = q_then_p pc $ Left qc
          goLeft (Right (R2 (Prd2 (AllClowns pc) qcj), j)) = q_then_p pc $ Right (qcj, j)
          goLeft (Right (L2 (Prd2 pcj (AllJokers qj)), j)) = case left $ Right (pcj, j) of
            Left (c, pcj1) -> Left (c, L2 $ Prd2 pcj1 $ AllJokers qj)
            Right pj -> Right $ Prd1 pj qj
          q_then_p pc q = case left q of
            Left (c, qcj) -> Left (c, R2 $ Prd2 (AllClowns pc) qcj)
            Right qj -> case left $ Left pc of
              Left (c, pcj) -> Left $ (c, L2 $ Prd2 pcj (AllJokers qj))
              Right pj -> Right $ Prd1 pj qj
  type DissectionContainer (Prd1 p q) = Sum2 (Prd2 (DissectionContainer p) (AllJokers q))
                                             (Prd2 (AllClowns p) (DissectionContainer q))
  plug x = \case
    L2 (Prd2 pcj (AllJokers q)) -> Prd1 (plug x pcj) q
    R2 (Prd2 (AllClowns p) qcj) -> Prd1 p $ plug x qcj

tmap :: Diss p c j => (j -> c) -> p j -> p c
tmap f = continue . right . Left
  where continue (Left (j, pcj)) = continue $ right $ Right (pcj, f j)
        continue (Right pc) = pc

main :: IO ()
main = do
  putStrLn . show $ listFsum $ list2listf [1..6 :: Int]
  putStrLn . show $ lfoldr (+) 0 $ list2listf [1..4 :: Int]
  putStrLn $ lfoldr (\a b -> show a ++ b) "." $ list2listf [1..4 :: Int]
  let expr val add = add (val 1) (add (add (val 2) (val 3)) (val 4))
  let exp0 = expr val0 add0
  putStrLn $ "0: " ++ pp0 exp0 ++ "=" ++ show (eval0 exp0)
  let exp1 = expr val1 add1
  putStrLn $ "1: " ++ pp1 exp1 ++ "=" ++ show (eval1 exp1)

module Arrows (
   Applicative
 , Arrow
 , ArrowApply
 , ArrowChoice
 , ArrowCircuit
 , ArrowLoop
 , Auto
 , AutoFunctor
 , BalTree(..)
 , Except
 , Hom
 , MapTrans
 , Monoid
 , NonDet
 , Pair
 , Reader
 , State
 , Stream(..)
 , StreamMap(..)
 , Writer
 , (&&&)
 , (***)
 , (*:*)
 , (*>*)
 , (+:+)
 , (+++)
 , (<*>)
 , (>>>)
 , (|||)
 , app
 , arr
 , apply
 , ask
 , balTreeToList
 , butterfly
 , delay
 , enumStream
 , first
 , get
 , idA
 , left
 , liftA2
 , listToBalTree
 , local
 , loop
 , mapA
 , mappend
 , mempty
 , put
 , right
 , rsh
 , runAuto
 , runNonDet
 , runReader
 , runState
 , runStreamMap
 , runWriter
 , scan
 , scanM
 , second
 , streamIota
 , streamToInfList
 , streamZip
 , tell
 , test
 , transpose
) where

class Arrow a where
  arr :: (b -> c) -> a b c
  (>>>) :: a b c -> a c d -> a b d
  first :: a b c -> a (b, d) (c, d)

second :: Arrow a => a b c -> a (d, b) (d, c)
second a = arr swap >>> first a >>> arr swap
  where swap ~(x, y) = (y, x)

idA :: Arrow a => a i i
idA = arr id

(*:*) :: (a -> a') -> (b -> b') -> (a, b) -> (a', b')
(f *:* g) ~(a, b) = (f a, g b)

assoc :: ((a, b), c) -> (a, (b, c))
assoc ~(~(a, b), c) = (a, (b, c))

unassoc :: (a, (b, c)) -> ((a, b), c)
unassoc ~(a, ~(b, c)) = ((a, b), c)

data Stream a = Cons a (Stream a)

instance Functor Stream where
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

streamZip (Cons a as) (Cons b bs) =
    Cons (a, b) $ streamZip as bs

streamToInfList (Cons a as) = a:streamToInfList as

streamIota n = Cons n $ streamIota $ n + 1

newtype StreamMap i o = SM (Stream i -> Stream o)

instance Functor (StreamMap i) where
  fmap f (SM g) = SM $ fmap f . g

runStreamMap s (SM f) = f s

instance Arrow StreamMap where
  arr = SM . fmap
  SM f >>> SM g = SM $ g . f
  -- first :: StreamMap i o -> StreamMap (i, d) (o, d)
  --          SM (Stream i -> Stream o)
  --                        -> SM (Stream (i, d)
  --                            -> Stream (o, d))
  first (SM f) = SM f'
    where f' isds = streamZip (f $ fmap fst isds)
                              (fmap snd isds)

class Arrow a => ArrowApply a where
  app :: a (a i o, i) o

instance ArrowApply StreamMap where
  -- app :: StreamMap (StreamMap i o, i) o
  --      = SM (  Stream (StreamMap i o, i)
  --           -> Stream o)
  --      = SM (  Stream (SM (Stream i -> Stream o), i)
  --           -> Stream o)
  app = SM f
    where f (Cons (SM f, i) xs) =
            let Cons o _ = f $ repeatCons i
            in Cons o $ runStreamMap xs app
          repeatCons x = Cons x $ repeatCons x

instance ArrowApply (->) where
  app = uncurry ($)

-- newtype State s i o = ST ((s, i) -> (s, o))
instance ArrowApply (State s) where
  -- app :: State s (State s i o, i) o
  --        ST (s, (ST (s, i) -> (s, o), i)) -> (s, o)
  app = ST $ \(s, (ST f, i)) -> f (s, i)

newtype State s i o = ST ((s, i) -> (s, o))

instance Arrow (->) where
  arr = id
  f >>> g = g . f
  first = (*:* id)

instance Arrow (State s) where
  arr f = ST (id *:* f)
  ST f >>> ST g = ST (g . f)
  -- first :: State s i o -> State s (i, z) (o, z)
  --          ST ((s, i) -> (s, o))
  --                      -> ST ((s, (i, z)) (s, (o, z)))
  first (ST f) = ST (assoc . (f *:* id) . unassoc)

get :: State s () s
get = ST (\(s, _) -> (s, s))

put :: s -> State s s ()
put s = ST (\(_, s) -> (s, ()))

runState :: s -> (State s i o) -> i -> (s, o)
runState initState (ST f) i = f (initState, i)

newtype NonDet i o = ND (i -> [o])

instance Arrow NonDet where
  arr = ND . ((:[]) .)
  ND f >>> ND g = ND $ \i -> [y | x <- f i, y <- g x]
  -- first :: NonDet i o -> NonDet (i, d) (o, d)
  --          ND (i -> [o])
  --                     -> ND ((i, d) -> [(o, d)])
  first (ND f) = ND $ \(i, d) -> [(x, d) | x <- f i]

runNonDet :: i -> NonDet i o -> [o]
runNonDet x (ND f) = f x

newtype MapTrans s i o = MT ((s -> i) -> (s -> o))

instance Arrow (MapTrans s) where
  -- arr :: (i -> o) -> MapTrans s i o
  arr f = MT (f .)
  -- MapTrans s i o >>> MapTrans s o p :: MapTrans s i p
  -- MT (s -> i) -> (s -> o)
  --                 -> MT (s -> o) -> (s -> p)
  --                                   -> MT (s -> i) -> (s -> p)
  MT f >>> MT g = MT $ g . f
  -- first :: MapTrans s i o -> MapTrans s (i, d) (o, d)
  --          MT (s -> i) -> (s -> o)
  --                         -> MT (s -> (i, d)) -> (s -> (o, d))
  first (MT f) = MT $ (zipMap . (f *:* id) . unzipMap)

zipMap :: (s -> a, s -> b) -> (s -> (a, b))
zipMap (f, g) s = (f s, g s)

unzipMap :: (s -> (a, b)) -> (s -> a, s -> b)
unzipMap f = (fst . f, snd . f)

(***) :: Arrow a => a b c -> a b' c' -> a (b, b') (c, c')
a *** b = first a >>> second b

(&&&) :: Arrow a => a i o -> a i p -> a i (o, p)
a &&& b = arr dup >>> (a *** b)
  where dup x = (x, x)

liftA2 :: Arrow a => (o -> p -> q) -> a i o -> a i p -> a i q
liftA2 op f g = (f &&& g) >>> arr (uncurry op)

newtype Reader r i o = R ((r, i) -> o)

instance Arrow (Reader r) where
  arr f = R $ \(_, x) -> f x
  R f >>> R g = R $ \(r, x) -> g (r, f (r, x))
  first (R f) = R $ \(r, (x, d)) -> (f (r, x), d)

runReader :: r -> i -> Reader r i o -> o
runReader r i (R f) = f (r, i)

ask :: Reader r i r
ask = R $ \(r, _) -> r

local :: (r -> s) -> Reader s i o -> Reader r i o
local modify (R f) = R $ \(r, i) -> f (modify r, i)

newtype Writer m i o = W (i -> (m, o))

instance Monoid m => Arrow (Writer m) where
  arr f = W $ \i -> (mempty, f i)
  W f >>> W g = W $ \i -> let (m1, o1) = f i
                              (m2, o2) = g o1
                          in (m1 `mappend` m2, o2)
  first (W f) = W $ \(i, d) -> let (m, o) = f i
                               in (m, (o, d))

runWriter :: i -> Writer m i o -> (m, o)
runWriter i (W f) = f i

tell :: Monoid m => m -> Writer m i ()
tell m = W $ const (m, ())

idAndTell :: Monoid m => m -> Writer m i i
idAndTell m = (tell m &&& arr id) >>> arr snd

(*>*) :: Arrow a => a i o -> (i' -> o') -> a (i, i') (o, o')
a *>* f = first a >>> arr (id *:* f)

instance ArrowApply (Reader r) where
  app = R $ \(r, (R f, i)) -> f (r, i)

-- This is dimap of bifunctor?
(+:+) :: (a -> a') -> (b -> b') -> Either a b -> Either a' b'
(f +:+ _) (Left lft) = Left $ f lft
(_ +:+ g) (Right rgt) = Right $ g rgt

class Arrow a => ArrowChoice a where
  left :: a i o -> a (Either i d) (Either o d)

instance ArrowChoice (->) where
  left f = g f
    where g f (Left x) = Left $ f x
          g _ (Right x) = Right x

right :: ArrowChoice a => a i o -> a (Either d i) (Either d o)
right a = mirror >>> left a >>> mirror
  where mirror = arr m
        m (Left a) = Right a
        m (Right a) = Left a

(|||) :: ArrowChoice a
      => a i o -> a i' o' -> a (Either i i') (Either o o')
f ||| g = left f >>> right g

(+++) :: ArrowChoice a
      => a i o -> a i' o -> a (Either i i') o
f +++ g = (f ||| g) >>> (arr untag)
  where untag (Left x) = x
        untag (Right x) = x

newtype Except a i o = E (a i (Either String o))

instance ArrowChoice a => Arrow (Except a) where
  -- arr :: (b -> c) -> Except a b c
  --       = (b -> c) -> E (a b (Either String c))
  arr = (E . arr) . (Right .) 
  -- (>>>) :: a b c -> a c d -> a b d
  --        = Except a b c -> Except a c d
  --       -> Except a b d
  --        = E (a b (Either String c))
  --                       -> E (a c (Either String d))
  --       -> E (a b (Either String d))
  E f >>> E g = E $ f >>> (left $ arr Left)
                      >>> (right g >>> arr unpackRight)
    where unpackRight ~(Right x) = x
  -- first :: a b c -> a (b, d) (c, d)
  --        = Except a b c -> Except a (b, d) (c, d)
  --        = E a b (Either String c)
  --                       -> E a (b, d) Either (String (c, d))
  first (E f) = E $ first f >>> arr dropSndForLeft
    where dropSndForLeft (Left x, _) = Left x
          dropSndForLeft (Right y, d) = Right (y, d)

listarr :: [a] -> Either () (a, [a])
listarr [] = Left ()
listarr (x:xs) = Right (x, xs)

mapA :: ArrowChoice a => a i o -> a [i] [o]
mapA a = arr listarr
     >>> (    (arr $ const [])
          +++ ((a *** mapA a) >>> (arr $ uncurry (:))))

streamTail :: Stream (Either i d)
           -> (Stream i -> Stream o)
           -> Stream (Either o d)
streamTail xs f = runStreamMap xs $ left $ SM f

filterLeft :: StreamMap (Either i d, x) (i, x)
filterLeft = SM f
  where f (Cons (Left x, y) xs) = Cons (x, y) $ f xs
        f (Cons (Right _, _) xs) = f xs

filterRight :: StreamMap (Either d i, x) (i, x)
filterRight = SM f
  where f (Cons (Right x, y) xs) = Cons (x, y) $ f xs
        f (Cons (Left _, _) xs) = f xs

enumStream :: Stream Integer
enumStream = go 0
  where go x = Cons x . go $ x + 1

mergeStreams :: Ord a => Stream (l, a) -> Stream (r, a) -> Stream (Either l r)
mergeStreams xx@(Cons (l, x) ls) yy@(Cons (r, y) ys)
  | x <= y = Cons (Left l) $ mergeStreams ls yy
  | otherwise = Cons (Right r) $ mergeStreams xx ys

instance ArrowChoice StreamMap where
  -- left :: StreamMap i o -> StreamMap (Either i d)
  --                                    (Either o d)
  --       = SM (Stream i -> Stream o)
  --                       -> SM (Stream (Either i o)
  --                           -> Stream (Either i d))
  left f = SM g
    where g lr =
            let enumed = streamZip lr enumStream
                left = runStreamMap enumed
                                    (filterLeft >>> first f)
                right = runStreamMap enumed filterRight
            in mergeStreams left right

instance ArrowChoice (State s) where
  -- left :: State s i o -> State s (Either i d) (Either o d)
  --         ST (s, i) -> (s, o)
  --                     -> ST (s, Either i d) -> (s, Eithero, d)
  left (ST f) =
      ST $ \(s, z) -> case z of
             Left i -> let (s', o) = f (s, i)
                       in (s', Left o)
             Right d -> (s, Right d)

instance ArrowChoice NonDet where
  -- left :: ND (i -> [o]) -> ND (Either i d -> [Either o d])
  left (ND f) = ND g
    where g (Left i) = map Left $ f i
          g (Right d) = [Right d]

test :: Arrow a => (i -> Bool) -> a i (Either i i)
test f = arr $ \x -> (if f x then Left x else Right x)

assocsum :: Either (Either a b) c -> Either a (Either b c)
assocsum (Left (Left a)) = Left a
assocsum (Left (Right b)) = Right $ Left b
assocsum (Right c) = Right $ Right c

distr :: (Either a b, c) -> Either (a, c) (b, c)
distr (Left a, c) = Left (a, c)
distr (Right b, c) = Right (b, c)

(<+>) :: ArrowChoice a => a i o -> a j p -> a (Either i j) (Either o p)
a <+> b = left a >>> right b

class Arrow a => ArrowLoop a where
  loop :: a (b, d) (c, d) -> a b c

trace :: ((b, d) -> (c, d)) -> b -> c
trace f b = let (c, d) = f (b, d) in c

instance ArrowLoop (->) where
  loop = trace

-- newtype State s i o = ST ((s, i) -> (s, o))
instance ArrowLoop (State s) where
  -- loop :: State s (i, d) (o, d) -> State s i o
  --       = ST ((s, (i, d)) -> (s, (o, d)))
  --                         ->  ST ((s, i) -> (s, o))
  loop (ST f) = ST $ trace $ unassoc . f . assoc

-- newtype MapTrans s i o = MT ((s -> i) -> (s -> o))
instance ArrowLoop (MapTrans s) where
  -- loop :: MapTrans s (i, d) (o, d) -> MapTrans s i o
  --       = MT ((s -> (i, d)) -> (s -> (o, d)))
  --                         ->  MT ((s -> i) -> (s -> o))
  -- f :: (s -> (i, d)) -> (s -> (o, d))
  -- g :: (s -> i) -> (s -> o)
  -- unzipMap . f :: (s -> (i, d)) -> (s -> o, s -> d)
  -- unzipMap . f . zipMap ::
  --                  (s -> i, s -> d) -> (s -> o, s -> d)
  -- trace $ ... :: (s -> i) -> (s -> o)
  loop (MT f) = MT . trace $ unzipMap . f . zipMap

newtype Auto i o = A (i -> (o, Auto i o))

runAuto :: Auto i o -> [i] -> [o]
runAuto _ [] = []
runAuto (A f) (i:is) = let (o, f') = f i in o:runAuto f' is

instance Arrow Auto where
  arr f = A g
    where g i = (f i, arr f)
  (A f) >>> (A g) = A $ h
    where h i = let (o, f') = f i
                    (p, g') = g o
                in (p, f' >>> g')
  first (A f) = A g
    where g (i, d) = let (o, f') = f i in ((o, d), first f')

instance ArrowLoop Auto where
  loop (A f) = A g
    where g i = let ((o, d), f') = f (i, d) in (o, loop f')

class ArrowLoop a => ArrowCircuit a where
  delay :: b -> a b b

counter :: ArrowCircuit a => a Bool Int
counter = loop $ arr upOrReset >>> delay (1234, 0)
  where upOrReset (True, _) = (0, 0)
        upOrReset (False, x) = (x + 1, x + 1)

counterAsyncReset :: ArrowCircuit a => a Bool Int
counterAsyncReset = loop $ second (delay 0) >>> arr upOrReset
  where upOrReset (True, _) = (0, 1)
        upOrReset (False, x) = (x, x + 1)

instance ArrowCircuit Auto where
  delay b = A $ \i -> (b, delay i)

instance ArrowLoop StreamMap where
  -- f :: SM ((i, d) -> (o, d))
  -- g :: SM (i -> o)
  loop (SM f) = SM g
    where g (Cons i is) =
              let Cons (y, d) ysds = f $ Cons (i, d) $ replaceOutputs is ysds
              in Cons y $ fmap fst ysds
          -- irrefutable pattern needed to avoid <<loop>>
          -- runtime error
          replaceOutputs (Cons x xs) ~(Cons (z, e) zses) =
              Cons (x, e) $ replaceOutputs xs zses

instance ArrowCircuit StreamMap where
  delay b = SM $ f b
    where f b (Cons x xs) = Cons b $ f x xs

data BalTree a = Zero a | Succ (BalTree (Pair a))
  deriving (Show, Eq)

instance Functor BalTree where
  fmap f (Zero a) = Zero $ f a
  fmap f (Succ bt) = Succ $ fmap (f *:* f) bt

instance Applicative BalTree where
  pure = Zero
  Zero f <*> bt = fmap f bt
  bt <*> Zero x = fmap ($ x) bt
  Succ f <*> Succ x =
    let lf = fmap fst f
        rf = fmap snd f
        lx = fmap fst x
        rx = fmap snd x
    in Succ $ joinBalTrees (lf <*> lx) (rf <*> rx)

joinBalTrees :: BalTree a -> BalTree a -> BalTree (Pair a)
joinBalTrees lft rgt = fmap (,) lft <*> rgt

type Pair a = (a, a)
data Hom a b = (a -> b) :&: Hom (Pair a) (Pair b)

apply :: Hom a b -> BalTree a -> BalTree b
apply (f :&: _) (Zero x) = Zero $ f x
apply (_ :&: fs) (Succ t) = Succ $ apply fs t

instance Arrow Hom where
  arr f = f :&: (arr $ \(a, b) -> (f a, f b))
  -- (>>>) :: a b c -> a c d -> a b d
  (f :&: fs) >>> (g :&: gs) = (f >>> g) :&: (fs >>> gs)
  -- first :: a b c -> a (b, d) (c, d)
  --        = Hom b c -> Hom (b, d) (c, d)
  --        = ((b -> c) :&: Hom (Pair b) (Pair c))
  --                  -> (    (b, d) -> (c, d)
  --                      :&: Hom (Pair (b, d))
  --                              (Pair (c, d)))
  --        = ((b -> c) :&: Hom (Pair b) (Pair c))
  --                  -> (    (b, d) -> (c, d)
  --                      :&: (    (Pair (b, d) -> Pair (c, d))
  --                           :&: ...))
  first (f :&: fs) = g :&: (arr transpose >>> first fs >>> arr transpose)
    where g (b, d) = (f b, d)

transpose ((b1, d1), (b2, d2)) = ((b1, b2), (d1, d2))

scan :: (a -> a -> a) -> a -> Hom a a
scan f n = id :&: scanPair f n

scanM :: Monoid a => Hom a a
scanM = scan mappend mempty

-- original implementation in paper uses arrow notation
-- proc (o, e) -> do
--   e' <- scan -< f o e
--   el <- rsh n -< e'
--   idA <- (f el o, e')

scanPair :: (a -> a -> a) -> a -> Hom (Pair a) (Pair a)
scanPair f n =
      (((arr $ \(o, e) -> f o e) >>> scan f n) &&& arr id)
  >>> (((arr $ \(e', (o, e)) -> e') >>> rsh n) &&& arr id)
  >>> (arr $ \(el, (e', (o, e))) -> (f el o, e'))

rsh :: b -> Hom b b
rsh v = const v
    :&: (    (    ((arr $ \(o, e) -> e) >>> rsh v)
              &&& (arr id))
         >>> (arr $ \(o', (o, e)) -> (o', o)))

butterfly :: (Pair a -> Pair a) -> Hom a a
butterfly f = id
          -- Original text in paper:
          --  proc (o, e) -> do
          --    o' <- butterfly f -< o
          --    e' <- butterfly f -< e
          --    idA -< f (o', e')
          :&: (    (    (first $ butterfly f) 
                    >>> (second $ butterfly f))
               >>> arr f)

rev :: Hom a a
rev = butterfly swap

swap (a, b) = (b, a)

unriffle :: Hom (Pair a) (Pair a)
unriffle = butterfly transpose

bisort :: Ord a => Hom a a
bisort = butterfly f
  where f (x, y) | x <= y    = (x, y)
                 | otherwise = (y, x)

newtype AutoFunctor a i o = AF (a i (o, AutoFunctor a i o))

instance Arrow a => Arrow (AutoFunctor a) where
  arr f = AF . arr $ \x -> (f x, arr f)
  --     AF (a i (c, AutoFunctor a i c))
  -- >>> AF (a c (o, AutoFunctor a c o))
  -- = AF (a i (o, AutoFunctor a i o))
  (AF f) >>> (AF g) = AF $ h f g
    where h :: Arrow a
            => a i (c, AutoFunctor a i c)
            -> a c (o, AutoFunctor a c o)
            -> a i (o, AutoFunctor a i o)
          h f g = f
              >>> (g *** idA)
              >>> (arr $ \((o, AF g'), AF f') ->
                              (o, AF $ h f' g'))
  -- first :: AutoFunctor a i o
  --       -> AutoFunctor a (i, d) (o, d)
  --       ~~ AF (a i (o, AutoFunctor a i o))
  --       -> AF (a (i, d) ((o, d), AutoFunctor a (i, d) (o, d)))
  first (AF f) = AF $ g f
    where g :: Arrow a
            => a i (o, AutoFunctor a i o)
            -> a (i, d) ((o, d), AutoFunctor a (i, d) (o, d))
          g f = first f >>> (arr $ \((o, a), d) -> ((o, d), first a))

instance ArrowChoice a => ArrowChoice (AutoFunctor a) where
  left (AF f) = AF $ h f
    -- f :: a i (o, AutoFunctor a i o)
    -- left f :: a (Either i d) (Either (o, AutoFunctor a i o) d)
    where h :: ArrowChoice a
            => a i (o, AutoFunctor a i o)
            -> a (Either i d)
                 (Either o d
                 , AutoFunctor a (Either i d) (Either o d))
          h f = left f >>> arr (g f)
          g :: ArrowChoice a
            => a i (o, AutoFunctor a i o)
            -> Either (o, AutoFunctor a i o) d
            -> (Either o d, AutoFunctor a (Either i d) (Either o d))
          g _ (Left (o, af)) = (Left o, left af)
          g f (Right d) = (Right d, left $ AF f)

instance ArrowLoop a => ArrowLoop (AutoFunctor a) where
  -- loop :: a (i, d) (o, d) -> a i o
  -- loop :: AutoFunctor a (i, d) (o, d)
  --      -> AutoFunctor a i o
  --      ~~ AF a (i, d) ((o, d), AutoFunctor a (i, d) (o, d))
  --      -> AF a i (o, AutoFunctor a i o)
  loop (AF f) = AF . loop $ g f
     where g :: ArrowLoop a
             => a (i, c) ((o, c), AutoFunctor a (i, c) (o, c))
             -> a (i, c) ((o, AutoFunctor a i o), c)
           g f = f
             >>> (arr $ \((o, c), af) -> ((o, loop af), c))

instance ArrowCircuit a => ArrowCircuit (AutoFunctor a) where
  -- delay :: b -> a b b
  --       ~~ b -> AutoFunctor a b b
  --       ~~ b -> AF a b (b, AutoFunctor a b b)
  delay b = AF $     delay b -- :: a b b
                 >>> (arr $ \o -> (o, delay o))

-- Return list with even and list with odd elements
-- (assuming there are an even number of elements)
splitList :: [a] -> Maybe [(a, a)]
splitList [] = Just []
splitList [x] = Nothing
splitList (x:y:xs) = splitList xs >>= return . ((x, y):)

-- Return first and second half of the list (assuming there
-- are an even number of elements)
halveList :: [a] -> Maybe ([a], [a])
halveList xs = doubleStep [] xs xs
  where doubleStep revacc (x:rest) (_:_:fast) =
            doubleStep (x:revacc) rest fast
        doubleStep revacc rest [] =
            Just (reverse revacc, rest)
        doubleStep _ _ [x] = Nothing

listToBalTree :: [a] -> Maybe (BalTree a)
listToBalTree [] = Nothing
listToBalTree [x] = Just $ Zero x
listToBalTree xs = do
  m <- splitList xs
  let f1 = map fst m
      s2 = map snd m
  lft <- listToBalTree f1
  rgt <- listToBalTree s2
  return . Succ $ joinBalTrees lft rgt

balTreeToList :: BalTree a -> [a]
balTreeToList (Zero x) = [x]
balTreeToList (Succ x) = go x
  where go :: BalTree (Pair a) -> [a]
        go = concatMap tupleToList . balTreeToList
        tupleToList :: Pair a -> [a]
        tupleToList (x, y) = [x, y]

{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
module Main (main) where

import Data.Kind (Type)

import Effect
import Penner (ioPenner, recordPenner)

class Category (cat :: k -> k -> Type) where
    ida :: forall (a :: k). cat a a
    (.>) :: forall (b :: k) (c :: k) (a :: k). cat b c -> cat a b -> cat a c

(>>>) :: forall k (a :: k) (b :: k) (c :: k) (cat :: k -> k-> Type).
             (Category cat) => cat a b -> cat b c -> cat a c
(>>>) = flip (.>)

class Category a => PreArrow a where
    arr :: (b -> c) -> a b c

class PreArrow a => Arrow a where
    first :: a b c -> a (b,d) (c, d)

class Arrow a => ChoiceArrow a where
    left :: a b c -> a (Either b d) (Either c d)

instance Category (->) where
    ida = id
    (.>) = (.)

instance PreArrow (->) where
    arr = id

instance Arrow (->) where
    first f = \(b, d) -> (f b, d)

instance ChoiceArrow (->) where
    left f (Left b) = Left $ f b
    left _ (Right d) = Right d

newtype AState s a b = AState { runAState :: (a, s) -> (b, s) }

instance Category (AState s) where
    ida = AState id
    AState f .> AState g = AState $ f . g

instance PreArrow (AState s) where
    arr f = AState $ first f

instance Arrow (AState s) where
    first (AState f) = AState $ \((a, d), s) -> let (b, s') = f (a, s) in ((b, d), s')

data FreerArrow e x y where
    Hom :: (x -> y) -> FreerArrow e x y
    Comp :: (x -> (a, c)) -> e a b -> FreerArrow e (b, c) y -> FreerArrow e x y

instance Category (FreerArrow e) where
    ida = Hom id
    Hom f .> Hom g = Hom $ f . g
    Comp g eff a .> Hom f = Comp (g . f) eff a
    f .> Comp g ggg b = Comp g ggg $ f .> b

instance PreArrow (FreerArrow e) where
    arr = Hom

instance Arrow (FreerArrow e) where
    first (Hom f) = Hom $ \(a, d) -> (f a, d)
    first (Comp f eff ar) =
        let g (x, d) = let (a, c) = f x in (a, (c, d))
         -- first ar :: FreerArrow e ((b, c), d) (y, d)
         in Comp g eff $ (Hom $ \(b, (c, d)) -> ((b, c), d)) >>> first ar

embed :: e x y -> FreerArrow e x y
embed eff = Comp (,()) eff $ arr fst

get :: URL -> [String] -> FreerArrow WebServiceOps () String
get url params = embed $ Get url params

post :: URL -> [String] -> FreerArrow WebServiceOps String ()
post url params = embed $ Post url params

echo :: URL -> URL -> [String] -> FreerArrow WebServiceOps () String
echo getUrl postUrl params = get getUrl params >>> dup >>> (first $ post postUrl params) >>> arr snd
    where dup = arr $ \x -> (x, x)

countEffects :: FreerArrow e a b -> Integer
countEffects (Hom _) = 0
countEffects (Comp _ _ cont) = 1 + countEffects cont

approximate :: Monoid m => (forall x y. e x y -> m) -> FreerArrow e a b -> m
approximate _ (Hom _) = mempty
approximate apx (Comp _ eff cont) = apx eff <> approximate apx cont

type p :-> q = forall a b. p a b -> q a b

interp :: Arrow arr => (e :-> arr) -> FreerArrow e a b -> arr a b
interp _ (Hom f) = arr f
interp eff2arr (Comp g eff cont) = arr g >>> (first $ eff2arr eff) >>> interp eff2arr cont

newtype ArrowM m i o = ArrowM { runArrowM :: i -> m o }

instance Monad m => Category (ArrowM m) where
    ida = ArrowM $ return
    ArrowM f .> ArrowM g = ArrowM $ \x -> g x >>= f

instance Monad m => PreArrow (ArrowM m) where
    arr f = ArrowM $ return . f

instance Monad m => Arrow (ArrowM m) where
    first (ArrowM a) = ArrowM $ \(x, d) -> fmap (, d) (a x)

instance Monad m => ChoiceArrow (ArrowM m) where
    left (ArrowM a) = ArrowM aLeft
        where aLeft (Left x) = fmap Left $ a x
              aLeft (Right x) = return $ Right x

interpWebServiceOpsIntoIO :: WebServiceOps a b -> ArrowM IO a b
interpWebServiceOpsIntoIO (Get url params) = ArrowM $ \() -> do
    putStrLn $ "curl '" <> url <> "?" <> show params <> "'"
    return $ "GET " <> url <> " with " <> (show $ length params) <> " parameters"
interpWebServiceOpsIntoIO (Post url params) = ArrowM $ \body ->
    putStrLn $ "echo '" <> body <> "' | curl -X POST '" <> url <> "?" <> show params <> "'"

interpWebServiceOpsIntoIntReader :: WebServiceOps a b -> ArrowM ((->) Int) a b
interpWebServiceOpsIntoIntReader (Get _ _) = ArrowM $ \() -> \x -> (show $ x + 2)
interpWebServiceOpsIntoIntReader (Post _ _) = ArrowM $ \_body -> const ()

interpWebServiceOpsIntoFuncsArrowInstance :: WebServiceOps a b -> a -> b
interpWebServiceOpsIntoFuncsArrowInstance (Get url params) = const $ "GET -> " <> url <> "?" <> show params
interpWebServiceOpsIntoFuncsArrowInstance (Post _ _) = const ()

second :: Arrow a => a b c -> a (d, b) (d, c)
second bc = arr swap >>> first bc >>> arr swap
    where swap (x, y) = (y, x)

(&&&) :: Arrow a => a b c -> a b d -> a b (c, d)
bc &&& bd = arr dup >>> first bc >>> second bd
    where dup x = (x, x)

(***) :: Arrow a => a b c -> a d e -> a (b, d) (c, e)
bc *** de = first bc >>> second de

data FreerChoiceArrow e x y where
    CHom :: (x -> y) -> FreerChoiceArrow e x y
    CComp :: (x -> Either (a, c) w) -> e a b -> FreerChoiceArrow e (Either (b, c) w) y -> FreerChoiceArrow e x y

instance Category (FreerChoiceArrow e) where
    ida = CHom id
    CHom f .> CHom g = CHom $ f . g
    CComp g eff a .> CHom f = CComp (g . f) eff a
    f .> CComp g ggg b = CComp g ggg $ f .> b

instance PreArrow (FreerChoiceArrow e) where
    arr = CHom

instance Arrow (FreerChoiceArrow e) where
    first (CHom f) = CHom $ \(a, d) -> (f a, d)
    first (CComp f eff cont) = CComp g eff $ CHom h >>> first cont
        where g (x, d) = case f x of
                             Left (a, c) -> Left (a, (c, d))
                             Right w -> Right (w, d)
              h (Left (a, (c, d))) = (Left (a, c), d)
              h (Right (w, d)) = (Right w, d)

instance ChoiceArrow (FreerChoiceArrow e) where
    left (CHom f) = CHom $ left f
    left (CComp g eff cont) = CComp h eff $ arr reassoc >>> left cont
        where h = unassoc . left g
              unassoc (Left (Left (a, c))) = Left (a, c)
              unassoc (Left (Right w)) = Right $ Left w
              unassoc (Right d) = Right $ Right d
              reassoc (Left (b, c)) = Left $ Left (b, c)
              reassoc (Right (Left w)) = Left $ Right w
              reassoc (Right (Right d)) = Right $ d

(+++) :: ChoiceArrow a => a b c -> a d e -> a (Either b d) (Either c e)
bc +++ de = left bc >>> right de

leftOrRight :: Either a a -> a
leftOrRight (Left x) = x
leftOrRight (Right x) = x
        
(|||) :: ChoiceArrow a => a b d -> a c d -> a (Either b c) d
bd ||| cd = (bd +++ cd) >>> arr leftOrRight

right :: ChoiceArrow a => a b c -> a (Either d b) (Either d c)
right a = arrSwap >>> left a >>> arrSwap
    where swapLR (Left x) = Right x
          swapLR (Right x) = Left x
          arrSwap = arr swapLR

cembed :: e x y -> FreerChoiceArrow e x y
cembed eff = CComp (Left . (,())) eff $ arr fstOfLeft
    where fstOfLeft (Left (x, _)) = x
          fstOfLeft (Right _) = error "cembed: expected Left, got Right"

cinterp :: ChoiceArrow arr => (e :-> arr) -> FreerChoiceArrow e a b -> arr a b
cinterp _ (CHom f) = arr f
cinterp eff2arr (CComp g eff cont) = arr g >>> (left $ first $ eff2arr eff) >>> cinterp eff2arr cont

overApproximate :: Monoid m => (forall x y. e x y -> m) -> FreerChoiceArrow e a b -> m
overApproximate _ (CHom _) = mempty
overApproximate apx (CComp _ eff cont) = apx eff <> overApproximate apx cont

main :: IO ()
main = do
    let prog = echo "https://example/com/get" "https://example/com/post" []
    putStrLn $ "Hello, Haskell! There are "
            <> (show $ countEffects prog)
            <> " effects."
    putStrLn $ "Ops = " <> (show $ approximate ((:[]) . nameThatOp) prog)
    result <- runArrowM (interp interpWebServiceOpsIntoIO prog) ()
    putStrLn $ "program result: '" <> result <> "'"
    let reader = runArrowM (interp interpWebServiceOpsIntoIntReader prog) ()
    putStrLn $ "reader 7 = " <> (show $ reader 7)
    putStrLn $ "reader 9 = " <> (show $ reader 9)
    let func = interp interpWebServiceOpsIntoFuncsArrowInstance prog
    putStrLn $ "func () = " <> (show $ func ())
    putStrLn "-- FreerChoiceArrow --"
    let cprog = (arr $ \x -> if x `mod` 2 == (0 :: Int)
                                 then Left ()
                                 else Right $ "short-cut GET for odd " <> show x)
            >>> (left $ cembed $ Get "https://x-even.com" [])
            >>> (arr leftOrRight)
            >>> arr (\x -> (x, x))
            >>> (first $ cembed $ Post "https://post.example.com" [])
            >>> arr snd
    result <- runArrowM (cinterp interpWebServiceOpsIntoIO cprog) 2
    putStrLn $ "program 2 result: '" <> result <> "'"
    result <- runArrowM (cinterp interpWebServiceOpsIntoIO cprog) 5
    putStrLn $ "program 5 result: '" <> result <> "'"
    putStrLn $ "Ops (at most) = " <> (show $ overApproximate ((:[]) . nameThatOp) cprog)
    pennerResult <- ioPenner "first body" "https://get.example.com/"
    putStrLn $ "ioPenner = " <> show pennerResult
    let recordResult = recordPenner "record" "https://get.record.com/"
    putStrLn $ "recordPenner = " <> show recordResult

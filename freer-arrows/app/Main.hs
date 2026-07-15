{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
module Main (main) where

import Data.Kind (Type)

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

instance Category (->) where
    ida = id
    (.>) = (.)

instance PreArrow (->) where
    arr = id

instance Arrow (->) where
    first f = \(b, d) -> (f b, d)

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

type URL = String

data WebServiceOps :: Type -> Type -> Type where
    Get :: URL -- ^ url
        -> [String] -- ^ params
        -> WebServiceOps () String -- ^ WebServiceOps without input producing a String
    Post :: URL -- ^ url
         -> [String] -- ^ params
         -> WebServiceOps String () -- ^ WebServiceOps accepting a String as body and producing ()

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

nameThatOp :: forall a b. WebServiceOps a b -> String
nameThatOp (Get _ _) = "GET"
nameThatOp (Post _ _) = "POST"

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

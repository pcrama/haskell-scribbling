{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
module Main (main) where

import Control.Arrow
import Control.Category as Cat
import Data.Kind (Type)

import Effect
import Penner (ioPenner, pennerArrow, recordPenner, CommandRecorder(..), CommandTree(Eff), GetPost(..))

newtype AState s a b = AState { runAState :: (a, s) -> (b, s) }

instance Category (AState s) where
    id = AState Prelude.id
    AState f . AState g = AState $ f Prelude.. g

instance Arrow (AState s) where
    arr f = AState $ first f
    first (AState f) = AState $ \((a, d), s) -> let (b, s') = f (a, s) in ((b, d), s')

data FreerArrow e x y where
    Hom :: (x -> y) -> FreerArrow e x y
    Comp :: (x -> (a, c)) -> e a b -> FreerArrow e (b, c) y -> FreerArrow e x y

instance Category (FreerArrow e) where
    id = Hom Prelude.id
    Hom f . Hom g = Hom $ f Prelude.. g
    Comp g eff a . Hom f = Comp (g Prelude.. f) eff a
    f . Comp g ggg b = Comp g ggg $ f Cat.. b

instance Arrow (FreerArrow e) where
    arr = Hom
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
echo getUrl postUrl params = Main.get getUrl params >>> dup >>> (first $ Main.post postUrl params) >>> arr snd
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

interpWebServiceOpsIntoIO :: WebServiceOps a b -> Kleisli IO a b
interpWebServiceOpsIntoIO (Get url params) = Kleisli $ \() -> do
    putStrLn $ "curl '" <> url <> "?" <> show params <> "'"
    return $ "GET " <> url <> " with " <> (show $ length params) <> " parameters"
interpWebServiceOpsIntoIO (Post url params) = Kleisli $ \body ->
    putStrLn $ "echo '" <> body <> "' | curl -X POST '" <> url <> "?" <> show params <> "'"

interpWebServiceOpsIntoIntReader :: WebServiceOps a b -> Kleisli ((->) Int) a b
interpWebServiceOpsIntoIntReader (Get _ _) = Kleisli $ \() -> \x -> (show $ x + 2)
interpWebServiceOpsIntoIntReader (Post _ _) = Kleisli $ \_body -> const ()

interpWebServiceOpsIntoFuncsArrowInstance :: WebServiceOps a b -> a -> b
interpWebServiceOpsIntoFuncsArrowInstance (Get url params) = const $ "GET -> " <> url <> "?" <> show params
interpWebServiceOpsIntoFuncsArrowInstance (Post _ _) = const ()

data FreerChoiceArrow e x y where
    CHom :: (x -> y) -> FreerChoiceArrow e x y
    CComp :: (x -> Either (a, c) w) -> e a b -> FreerChoiceArrow e (Either (b, c) w) y -> FreerChoiceArrow e x y

instance Category (FreerChoiceArrow e) where
    id = CHom Prelude.id
    CHom f . CHom g = CHom $ f Prelude.. g
    CComp g eff a . CHom f = CComp (g Prelude.. f) eff a
    f . CComp g ggg b = CComp g ggg $ f Cat.. b

instance Arrow (FreerChoiceArrow e) where
    arr = CHom
    first (CHom f) = CHom $ \(a, d) -> (f a, d)
    first (CComp f eff cont) = CComp g eff $ CHom h >>> first cont
        where g (x, d) = case f x of
                             Left (a, c) -> Left (a, (c, d))
                             Right w -> Right (w, d)
              h (Left (a, (c, d))) = (Left (a, c), d)
              h (Right (w, d)) = (Right w, d)

instance ArrowChoice (FreerChoiceArrow e) where
    left (CHom f) = CHom $ left f
    left (CComp g eff cont) = CComp h eff $ arr reassoc >>> left cont
        where h = unassoc Prelude.. left g
              unassoc (Left (Left (a, c))) = Left (a, c)
              unassoc (Left (Right w)) = Right $ Left w
              unassoc (Right d) = Right $ Right d
              reassoc (Left (b, c)) = Left $ Left (b, c)
              reassoc (Right (Left w)) = Left $ Right w
              reassoc (Right (Right d)) = Right $ d

leftOrRight :: Either a a -> a
leftOrRight (Left x) = x
leftOrRight (Right x) = x

cembed :: e x y -> FreerChoiceArrow e x y
cembed eff = CComp (Left Prelude.. (,())) eff $ arr fstOfLeft
    where fstOfLeft (Left (x, _)) = x
          fstOfLeft (Right _) = error "cembed: expected Left, got Right"

cinterp :: ArrowChoice arr => (e :-> arr) -> FreerChoiceArrow e a b -> arr a b
cinterp _ (CHom f) = arr f
cinterp eff2arr (CComp g eff cont) = arr g >>> (left $ first $ eff2arr eff) >>> cinterp eff2arr cont

overApproximate :: Monoid m => (forall x y. e x y -> m) -> FreerChoiceArrow e a b -> m
overApproximate _ (CHom _) = mempty
overApproximate apx (CComp _ eff cont) = apx eff <> overApproximate apx cont

data WebServiceOpsWithDyn :: Type -> Type -> Type where
  GetWD :: URL -> [String] -> WebServiceOpsWithDyn () String
  GetDynWD :: [String] -> WebServiceOpsWithDyn URL String
  PostWD :: URL -> [String] -> WebServiceOpsWithDyn String ()

instance GetPost (FreerChoiceArrow WebServiceOpsWithDyn) where
  get url params = cembed $ GetWD url params
  getDyn params = cembed $ GetDynWD params
  post url params = cembed $ PostWD url params

interpWebServiceOpsIntoCommandRecorder :: WebServiceOpsWithDyn a b -> CommandRecorder String a b
interpWebServiceOpsIntoCommandRecorder (GetWD url _) = CommandRecorder $ Eff $ "Get " <> url
interpWebServiceOpsIntoCommandRecorder (PostWD url _) = CommandRecorder $ Eff $ "Post " <> url
interpWebServiceOpsIntoCommandRecorder (GetDynWD _) = CommandRecorder $ Eff $ "GetDyn"
  
main :: IO ()
main = do
    let prog = echo "https://example/com/get" "https://example/com/post" []
    putStrLn $ "Hello, Haskell! There are "
            <> (show $ countEffects prog)
            <> " effects."
    putStrLn $ "Ops = " <> (show $ approximate ((:[]) Prelude.. nameThatOp) prog)
    result <- runKleisli (interp interpWebServiceOpsIntoIO prog) ()
    putStrLn $ "program result: '" <> result <> "'"
    let reader = runKleisli (interp interpWebServiceOpsIntoIntReader prog) ()
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
    result <- runKleisli (cinterp interpWebServiceOpsIntoIO cprog) 2
    putStrLn $ "program 2 result: '" <> result <> "'"
    result <- runKleisli (cinterp interpWebServiceOpsIntoIO cprog) 5
    putStrLn $ "program 5 result: '" <> result <> "'"
    putStrLn $ "Ops (at most) = " <> (show $ overApproximate ((:[]) Prelude.. nameThatOp) cprog)
    pennerResult <- ioPenner "first body" "https://get.example.com/"
    putStrLn $ "ioPenner = " <> show pennerResult
    let recordResult = recordPenner "record" "https://get.record.com/"
    putStrLn $ "recordPenner = " <> show recordResult
    putStrLn $ "freer penner doesn't reflect the first or left 'structure'\n\
               \             = " <> show (cinterp interpWebServiceOpsIntoCommandRecorder
                                                $ pennerArrow "firstBody" "http://get.com")

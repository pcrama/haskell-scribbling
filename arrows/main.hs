import Arrows

intIntegrator :: StreamMap Int Int
intIntegrator = SM $ f 0
  where f a (Cons x xs) = Cons a $ f (a + x) xs

intDiff :: StreamMap Int Int
intDiff = SM $ f 0
  where f p (Cons x xs) = Cons (x - p) $ f x xs

resampler = SM output
  where output (Cons x xs) = Cons x (swallow xs)
        swallow (Cons x xs) = output xs

counter :: ArrowCircuit a => a Bool Int
counter = loop $ pure upOrReset >>> delay (1234, 0)
  where upOrReset (True, _) = (0, 0)
        upOrReset (False, x) = (x + 1, x + 1)

counterAsyncReset :: ArrowCircuit a => a Bool Int
counterAsyncReset = loop $ second (delay 0) >>> pure upOrReset
  where upOrReset (True, _) = (0, 1)
        upOrReset (False, x) = (x, x + 1)

rev :: Hom a a
rev = butterfly swap

swap (a, b) = (b, a)

unriffle :: Hom (Pair a) (Pair a)
unriffle = butterfly transpose

bisort :: Ord a => Hom a a
bisort = butterfly f
  where f (x, y) | x <= y    = (x, y)
                 | otherwise = (y, x)

data Count a i o = Count Int (a i o)

instance Arrow a => Arrow (Count a) where
  pure = Count 0 . pure 
  (Count n1 f) >>> (Count n2 g) = Count (n1 + n2) $ f >>> g
  first (Count n f) = Count n $ first f

instance ArrowChoice a => ArrowChoice (Count a) where
  -- left :: a i o -> a (Either i d) (Either o d)
  left (Count n f) = Count n $ left f

instance ArrowLoop a => ArrowLoop (Count a) where
  loop (Count n f) = Count n $ loop f

bt0 :: BalTree Int
bt0 = Succ $ Succ $ Succ $ Zero (((0, 0), (0, 0)), ((0, 0), (0, 0)))

bt1 = apply (rsh 7) bt0
bt2 = apply (rsh 6) bt1
bt3 = apply (rsh 5) bt2
bt4 = apply (rsh 4) bt3
bt5 = apply (rsh 3) bt4
bt6 = apply (rsh 2) bt5
bt7 = apply (rsh 1) bt5

main = do
  putStrLn . show $ bt1
  putStrLn . show $ bt2
  putStrLn . show $ bt3
  putStrLn . show $ bt4
  putStrLn . show $ bt5
  putStrLn . show $ bt6
  putStrLn . show . ([2, 3, 4, 5, 6, 7, 0, 0] ==) $ balTreeToList bt6
  putStrLn . maybe "?" (show . balTreeToList . apply (scan (+) 0)) $ listToBalTree [1..8]
  putStrLn . maybe "?" (show . balTreeToList . apply (scan (+) 0)) $ listToBalTree [1..7]
  putStrLn . maybe "?" (show . balTreeToList . apply bisort) $ listToBalTree [8, 9, 10, 11, 12, 13, 14, 15, 7, 6, 5, 4, 3, 2, 2.5, 2.25]

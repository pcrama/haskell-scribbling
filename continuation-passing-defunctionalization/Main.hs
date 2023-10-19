module Subtractorial
where

subtractorial_rec :: Int -> Int
subtractorial_rec n
  | n > 0 = n - subtractorial_rec (n - 1)
  | otherwise = 0


subtractorial_cont :: Int -> Int
subtractorial_cont = subtractorial_cont_ id

subtractorial_cont_ :: (Int -> Int) -> Int -> Int
subtractorial_cont_ k n
  | n > 0 = subtractorial_cont_ (k . \r -> n - r) $ n - 1
  | otherwise = k 0


subtractorial_defunct :: Int -> Int
subtractorial_defunct = subtractorial_defunct_ id

data DF = Ret | NMinus Int DF

defunct_apply :: DF -> Int -> Int
defunct_apply Ret = id
defunct_apply (NMinus n df) = \r -> n - r

subtractorial_defunct_ :: (Int -> Int) -> Int -> Int
subtractorial_defunct_ k n
  | n > 0 = subtractorial_defunct_ (\r -> k $ n - r) $ n - 1
  | otherwise = k 0

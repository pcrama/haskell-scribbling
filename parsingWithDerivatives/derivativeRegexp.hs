data Reg a = Reg
  { deriveReg :: a -> Reg a
  , nullableReg :: Bool
  }

eps :: Reg a
eps = Reg { deriveReg = const empty, nullableReg = True }

empty :: Reg a
empty = Reg { deriveReg = const empty, nullableReg = False }

any :: Reg a
any = Reg { deriveReg = const eps, nullableReg = False }

match1 :: Eq a => a -> Reg a
match1 x = Reg { deriveReg = f, nullableReg = False }
  where f y | x == y = eps
            | otherwise = empty

(+*+) :: Reg a -> Reg a -> Reg a
f +*+ g = Reg { deriveReg = derive, nullableReg = nullableReg f && nullableReg g }
  where derive a | nullableReg f = (deriveReg f a +*+ g) |*| deriveReg g a
                 | otherwise = deriveReg f a +*+ g

(|*|) :: Reg a -> Reg a -> Reg a
f |*| g = Reg { deriveReg = derive, nullableReg = nullableReg f || nullableReg g }
  where derive a = deriveReg f a |*| deriveReg g a

bool :: a -> a -> Bool -> a
bool f _ False = f
bool _ t True = t

digit :: Reg Char
digit = Reg { deriveReg = bool empty eps . (`elem` "0123456789"), nullableReg = False }

kleene :: Reg a -> Reg a
kleene z@(Reg { deriveReg = d }) = Reg { deriveReg = (+*+ kleene z) . (d $), nullableReg = True }

pleese :: Reg a -> Reg a -- Kleene +
pleese z = z +*+ kleene z

oneOrZero :: Reg a -> Reg a
oneOrZero Reg { deriveReg = d } = Reg { deriveReg = d, nullableReg = True }

seque :: Eq a => [a] -> Reg a
seque [] = eps
seque (x:xs) = Reg { deriveReg = (bool eps $ seque xs) . (== x), nullableReg = False }

domatch :: [a] -> Reg a -> Bool
domatch [] r = nullableReg r
domatch (a:as) r = domatch as $ deriveReg r a

r1 = match1 'a'
r2 = r1 +*+ any +*+ r1
r3 = r2 |*| (match1 'b' +*+ match1 'c')
r4 = r3 +*+ r2
r5 = kleene r3 +*+ ((digit +*+ r5) |*| pleese r2 |*| seque "bcbcaba4")
re = kleene digit +*+ (oneOrZero $ match1 'a')

main :: IO ()
main = do
  putStrLn . show $ ("a", True == domatch "a" r1)
  putStrLn . show $ ("b", False == domatch "b" r1)
  putStrLn . show $ ("aa", False == domatch "aa" r1)
  putStrLn . show $ ("aaa", True == domatch "aaa" r2)
  putStrLn . show $ ("aza", True == domatch "aza" r2)
  putStrLn . show $ ("azaz", False == domatch "azaz" r2)
  putStrLn . show $ ("aab", False == domatch "aab" r2)
  putStrLn . show $ ("baa", False == domatch "baa" r2)
  putStrLn . show $ ("aaa", True == domatch "aaa" r3)
  putStrLn . show $ ("aza", True == domatch "aza" r3)
  putStrLn . show $ ("azaz", False == domatch "azaz" r3)
  putStrLn . show $ ("aab", False == domatch "aab" r3)
  putStrLn . show $ ("baa", False == domatch "baa" r3)
  putStrLn . show $ ("bc", True == domatch "bc" r3)
  putStrLn . show $ ("bca", False == domatch "bca" r3)
  putStrLn . show $ ("bcaaa", False == domatch "bcaaa" r3)
  putStrLn . show $ ("aaa", False == domatch "aaa" r4)
  putStrLn . show $ ("aza", False == domatch "aza" r4)
  putStrLn . show $ ("azaz", False == domatch "azaz" r4)
  putStrLn . show $ ("aab", False == domatch "aab" r4)
  putStrLn . show $ ("baa", False == domatch "baa" r4)
  putStrLn . show $ ("bc", False == domatch "bc" r4)
  putStrLn . show $ ("bca", False == domatch "bca" r4)
  putStrLn . show $ ("bcaaa", True == domatch "bcaaa" r4)
  putStrLn . show $ ("bcbc", False == domatch "bcbc" r4)
  putStrLn . show $ ("abaaba", True == domatch "abaaba" r4)
  putStrLn . show $ ("bc", False == domatch "bc" r5)
  putStrLn . show $ ("bca", False == domatch "bca" r5)
  putStrLn . show $ ("bcaaa", True == domatch "bcaaa" r5)
  putStrLn . show $ ("bcbc", False == domatch "bcbc" r5)
  putStrLn . show $ ("abaaba", True == domatch "abaaba" r5)
  putStrLn . show $ ("ababcaba", True == domatch "ababcaba" r5)
  putStrLn . show $ ("ababcaaaa", False == domatch "ababcaaaa" r5)
  putStrLn . show $ ("ababcaaaarabcbcaba4", True == domatch "ababcaaaarabcbcaba4" r5)
  putStrLn . show $ ("ababcaba8ababcbcbcabaabaaba", True == domatch "ababcaba8ababcbcbcabaabaaba" r5)
  putStrLn . show $ ("11111112222111a", True == domatch "11111112222111a" re)
  putStrLn . show $ ("11111112222111", True == domatch "11111112222111" re)
  putStrLn . show $ ("11111112222111aa", False == domatch "11111112222111aa" re)

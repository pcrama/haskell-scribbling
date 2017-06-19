newtype State s a = State { runState :: s -> (s, a) }

put :: s -> State s ()
put x = State $ \_ -> (x, ())

fetch :: State s s
fetch = State $ \s -> (s, s)

instance Functor (State s) where
  fmap f (State t) = State $ \s -> let (s', a') = t s in (s', f a')

instance Applicative (State s) where
  pure f = State $ \s -> (s, f)
  (State f) <*> (State g) = State h
    where h s = (s'', f' g')
            where (s', f') = f s
                  (s'', g') = g s'

instance Monad (State s) where
  return = pure
  (State x) >>= g = State h
    where h s = g' s'
            where (s', x') = x s
                  State g' = g x'

type Capture a = [(Int, [a])]

data RegStatus = Nullable | NotNullableNotEmpty | Empty
  deriving (Eq, Show)

data Reg a = Reg
  { deriveReg :: a -> State (Capture a) (Reg a)
  , statusReg :: RegStatus
  }

nullableReg :: Reg a -> Bool
nullableReg Reg { statusReg = x } = x == Nullable

eps :: Reg a
eps = Reg { deriveReg = pure . const empty, statusReg = Nullable }

empty :: Reg a
empty = Reg { deriveReg = pure . const empty, statusReg = Empty }

any :: Reg a
any = Reg { deriveReg = pure . const eps, statusReg = NotNullableNotEmpty }

match1 :: Eq a => a -> Reg a
match1 x = Reg { deriveReg = pure . f, statusReg = NotNullableNotEmpty }
  where f y | x == y = eps
            | otherwise = empty

notmatch1 :: Eq a => a -> Reg a
notmatch1 x = Reg { deriveReg = pure . f, statusReg = NotNullableNotEmpty }
  where f y | x == y = empty
            | otherwise = eps

-- notmatch (match1 'a') -> "", "b", "abc"...
-- match1 'a' +*+ notmatch (match1 'b') +*+ match1 'c' -> "ac", "azzzzxc", "anc"...
notmatch :: Reg a -> Reg a
notmatch r@Reg { deriveReg = d, statusReg = s }
               | s == Empty = kleene any
               | otherwise = Reg { deriveReg = complementDerive d, statusReg = complementStatus s }
               where complementDerive d c = d c >>= return . notmatch
                     complementStatus Nullable = NotNullableNotEmpty
                     complementStatus NotNullableNotEmpty = Nullable
                     complementStatus Empty = Nullable -- Not really needed

(+*+) :: Reg a -> Reg a -> Reg a
f@(Reg { statusReg = sf }) +*+ g@(Reg { statusReg = sg })
  | sf == Empty || sg == Empty = empty
  | otherwise = Reg { deriveReg = derive
                    , statusReg = bool NotNullableNotEmpty Nullable
                                     $ sf == Nullable && sg == Nullable }
  where derive a | nullableReg f = do { df <- deriveReg f a; dg <- deriveReg g a; return $ (df +*+ g) |*| dg }
                 | otherwise = do { df <- deriveReg f a; return $ df +*+ g }

(|*|) :: Reg a -> Reg a -> Reg a
f@(Reg { statusReg = sf }) |*| g@(Reg { statusReg = sg })
  | sf == Empty = g
  | sg == Empty = f
  | otherwise = Reg { deriveReg = derive
                    , statusReg = bool NotNullableNotEmpty Nullable
                                     $ nullableReg f || nullableReg g }
  where derive a = (fmap (|*|) $ deriveReg f a) <*> deriveReg g a

(&*&) :: Reg a -> Reg a -> Reg a
f@(Reg { statusReg = sf }) &*& g@(Reg { statusReg = sg })
  | sf == Empty = empty
  | sg == Empty = empty
  | otherwise = Reg { deriveReg = derive
                    , statusReg = bool NotNullableNotEmpty Nullable
                                     $ nullableReg f && nullableReg g }
  where derive a = (fmap (&*&) $ deriveReg f a) <*> deriveReg g a

bool :: a -> a -> Bool -> a
bool f _ False = f
bool _ t True = t

digit :: Reg Char
digit = Reg { deriveReg = pure . bool empty eps . (`elem` "0123456789")
            , statusReg = NotNullableNotEmpty }

letter :: Reg Char
letter = Reg { deriveReg = pure . bool empty eps . isLetter
             , statusReg = NotNullableNotEmpty }
  where isLetter x = (x == '_')
                  || ('A' <= x) && (x <= 'Z')
                  || ('a' <= x) && (x <= 'z')

kleene :: Reg a -> Reg a
kleene z@(Reg { deriveReg = d }) = Reg { deriveReg = fmap (+*+ kleene z) . d
                                       , statusReg = Nullable }

pleese :: Reg a -> Reg a -- Kleene +
pleese z = z +*+ kleene z

oneOrZero :: Reg a -> Reg a
oneOrZero Reg { deriveReg = d } = Reg { deriveReg = d, statusReg = Nullable }

updateAssocList :: Eq k => k -> (a -> a) -> [(k, a)] -> [(k, a)]
updateAssocList key upd = foldr consOrUpdate []
  where consOrUpdate z@(k, old) rest | k == key = (k, upd old):rest
                                     | otherwise = z:rest

-- `when' isn't in the prelude and I can't import modules in repl.it
when :: Monad m => Bool -> m () -> m ()
when True m = m
when False _ = return ()

capture :: Int -> Reg a -> Reg a
capture key Reg { deriveReg = d, statusReg = n } = Reg { deriveReg = wrap d, statusReg = n }
  where wrap :: (a -> State (Capture a) (Reg a)) -> a -> State (Capture a) (Reg a)
        wrap d c = do
                     cap <- fetch
                     let maybeV = lookup key cap
                     case maybeV of
                       Just _ -> return empty -- match fails because of duplicate keys
                       Nothing -> do
                            dr <- d c
                            when (statusReg dr /= Empty) $ put $ (key, [c]):cap
                            return $ contWrap dr
        contWrap :: Reg a -> Reg a
        contWrap r@(Reg { statusReg = s }) = Reg { deriveReg = derivWrap r
                                                 , statusReg = s }
        derivWrap :: Reg a -> a -> State (Capture a) (Reg a)
        derivWrap d c = do
                         cap <- fetch
                         let maybeV = lookup key cap
                         case maybeV of
                           Just _ -> do
                                dr <- deriveReg d $ c
                                if (statusReg dr /= Empty)
                                then do
                                       put $ updateAssocList key (++[c]) cap
                                       return $ contWrap dr
                                else return dr
                           Nothing -> return empty -- match fails because key not found
                         
seque :: Eq a => [a] -> Reg a
seque = foldr (\x xs -> match1 x +*+ xs) eps
--seque (x:xs) = Reg { deriveReg = (bool eps $ seque xs) . (== x), statusReg = NotNullableNotEmpty }

domatch :: [a] -> Reg a -> (Capture a, Bool)
domatch xs r = runState (go xs r) $ []
  where go :: [a] -> Reg a -> State (Capture a) Bool
        go [] r = pure $ nullableReg r
        go (a:as) r = deriveReg r a >>= go as

r1 = match1 'a'
r2 = r1 +*+ any +*+ r1
r3 = r2 |*| (match1 'b' +*+ match1 'c')
r4 = r3 +*+ r2
r5 = kleene r3 +*+ ((digit +*+ kleene r3) |*| pleese r2 |*| seque "bcbcaba4")
re = kleene digit +*+ (oneOrZero $ match1 'a')
r6 =     capture 1 (letter +*+ kleene (letter |*| digit))
     +*+ kleene (match1 ' ') +*+ match1 '=' +*+ kleene (match1 ' ') +*+ match1 '"'
     +*+ (capture 2 $ kleene $ notmatch1 '"')
     +*+ match1 '"'
r7 =     capture 1 ((letter +*+ (kleene $ letter |*| digit)) &*& (notmatch $ seque "if" |*| seque "case" |*| seque "while" |*| seque "end" |*| seque "then" |*| seque "else"))
     +*+ kleene (match1 ' ') +*+ match1 '=' +*+ kleene (match1 ' ') +*+ match1 '"'
     +*+ (capture 2 $ kleene $ notmatch1 '"' |*| (match1 '\\' +*+ any))
     +*+ match1 '"'

testRegexp :: (Show a, Eq a) => [a] -> Reg a -> Capture a -> Bool -> (Bool, Capture a, String)
testRegexp input regexp expCapture expMatch = (match,
                                               capture,
                                               comment)
  where (capture, match) = domatch input regexp
        compareCapture obs exp
          | length obs /= length exp = False
          | otherwise = all (findAndCompare exp) obs
        findAndCompare alist (key, val) =
          maybe False (val ==) $ lookup key alist
        comment = let success = match == expMatch && compareCapture capture expCapture
                  in (bool "FAIL" "ok" success)
                  ++ ": " ++ show input

main :: IO ()
main = do
  putStrLn . show $ testRegexp "a" r1 [] True
  putStrLn . show $ testRegexp "a" r1 [] True
  putStrLn . show $ testRegexp "b" r1 [] False
  putStrLn . show $ testRegexp "aa" r1 [] False
  putStrLn . show $ testRegexp "aaa" r2 [] True
  putStrLn . show $ testRegexp "aza" r2 [] True
  putStrLn . show $ testRegexp "azaz" r2 [] False
  putStrLn . show $ testRegexp "aab" r2 [] False
  putStrLn . show $ testRegexp "baa" r2 [] False
  putStrLn . show $ testRegexp "aaa" r3 [] True
  putStrLn . show $ testRegexp "aza" r3 [] True
  putStrLn . show $ testRegexp "azaz" r3 [] False
  putStrLn . show $ testRegexp "aab" r3 [] False
  putStrLn . show $ testRegexp "baa" r3 [] False
  putStrLn . show $ testRegexp "bc" r3 [] True
  putStrLn . show $ testRegexp "bca" r3 [] False
  putStrLn . show $ testRegexp "bcaaa" r3 [] False
  putStrLn . show $ testRegexp "aaa" r4 [] False
  putStrLn . show $ testRegexp "aza" r4 [] False
  putStrLn . show $ testRegexp "azaz" r4 [] False
  putStrLn . show $ testRegexp "aab" r4 [] False
  putStrLn . show $ testRegexp "baa" r4 [] False
  putStrLn . show $ testRegexp "bc" r4 [] False
  putStrLn . show $ testRegexp "bca" r4 [] False
  putStrLn . show $ testRegexp "bcaaa" r4 [] True
  putStrLn . show $ testRegexp "bcbc" r4 [] False
  putStrLn . show $ testRegexp "abaaba" r4 [] True
  putStrLn . show $ testRegexp "bc" r5 [] False
  putStrLn . show $ testRegexp "bca" r5 [] False
  putStrLn . show $ testRegexp "bcaaa" r5 [] True
  putStrLn . show $ testRegexp "bcbc" r5 [] False
  putStrLn . show $ testRegexp "abaaba" r5 [] True
  putStrLn . show $ testRegexp "ababcaba" r5 [] True
  putStrLn . show $ testRegexp "ababcaaaa" r5 [] False
  putStrLn . show $ testRegexp "ababcaaaarabcbcaba4" r5 [] True
  putStrLn . show $ testRegexp "ababcaba8ababcbcbcabaabaaba" r5 [] True
  putStrLn . show $ testRegexp "11111112222111a" re [] True
  putStrLn . show $ testRegexp "11111112222111" re [] True
  putStrLn . show $ testRegexp "11111112222111aa" re [] False
  putStrLn . show $ testRegexp "an_identifier =\"value\"" r6 [(1, "an_identifier"), (2, "value")] True
  putStrLn . show $ testRegexp "2an_identifier =\"value\"" r6 [] False
  putStrLn . show $ testRegexp "a" (notmatch $ match1 'a') [] False
  putStrLn . show $ testRegexp "aa" (notmatch $ match1 'a') [] True
  putStrLn . show $ testRegexp "b" (notmatch $ match1 'a') [] True
  putStrLn . show $ testRegexp "ba" (notmatch $ match1 'a') [] True
  putStrLn . show $ testRegexp "ba" (notmatch $ seque "ba") [] False
  putStrLn . show $ testRegexp "aa" (notmatch $ seque "ba") [] True
  putStrLn . show $ testRegexp "bb" (notmatch $ seque "ba") [] True
  putStrLn . show $ testRegexp "cc" (notmatch $ seque "cc" +*+ (kleene $ match1 'c')) [] False
  putStrLn . show $ testRegexp "cc" (notmatch $ seque "cc" +*+ (pleese $ match1 'c')) [] True
  putStrLn . show $ testRegexp "c" (notmatch $ seque "cc" +*+ (kleene $ match1 'c')) [] True
  putStrLn . show $ testRegexp "ccccccccccca" (notmatch $ seque "cc" +*+ (kleene $ match1 'c')) [] True
  putStrLn . show $ testRegexp "d" (notmatch $ match1 'a' |*| match1 'b') [] True
  putStrLn . show $ testRegexp "a" (notmatch $ match1 'a' |*| match1 'b') [] False
  putStrLn . show $ testRegexp "b" (notmatch $ match1 'a' |*| match1 'b') [] False
  putStrLn . show $ testRegexp "ab" (notmatch $ match1 'a' |*| match1 'b') [] True
  putStrLn . show $ testRegexp "d" (notmatch $ match1 'a' |*| match1 'b' |*| match1 'c') [] True
  putStrLn . show $ testRegexp "a" (notmatch $ match1 'a' |*| match1 'b' |*| match1 'c') [] False
  putStrLn . show $ testRegexp "b" (notmatch $ match1 'a' |*| match1 'b' |*| match1 'c') [] False
  putStrLn . show $ testRegexp "c" (notmatch $ match1 'a' |*| match1 'b' |*| match1 'c') [] False
  putStrLn . show $ testRegexp "ab" (notmatch $ match1 'a' |*| match1 'b' |*| match1 'c') [] True
  putStrLn . show $ testRegexp "z" (notmatch $ digit) [] True
  putStrLn . show $ testRegexp "1z" (notmatch $ digit) [] True
  putStrLn . show $ testRegexp "1" (notmatch $ digit) [] False
  putStrLn . show $ testRegexp "y" (notmatch $ digit |*| letter) [] False
  putStrLn . show $ testRegexp "2y" (notmatch $ digit |*| letter) [] True
  putStrLn . show $ testRegexp "2" (notmatch $ digit |*| letter) [] False
  putStrLn . show $ testRegexp "2" (notmatch $ letter |*| digit) [] False
  putStrLn . show $ testRegexp "+" (notmatch $ digit |*| letter) [] True
  putStrLn . show $ testRegexp "+" (notmatch $ kleene $ digit |*| letter) [] True
  putStrLn . show $ testRegexp "+2" (notmatch $ kleene $ digit |*| letter) [] True
  putStrLn . show $ testRegexp "2+" (notmatch $ kleene $ digit |*| letter) [] True
  putStrLn . show $ testRegexp "2an_identifier =\"value7\"" r7 [] False
  putStrLn . show $ testRegexp "an_identifier =\"value7\"" r7 [(1, "an_identifier"), (2, "value7")] True
  putStrLn . show $ testRegexp "a+-*/ =\"value\"" r7 [(1, "a")] False
  putStrLn . show $ testRegexp "if=\"keyword\"" r7 [(1, "if")] False
  putStrLn . show $ testRegexp "if_if=\"keyword" r7 [(1, "if_if"), (2, "keyword")] False
  putStrLn . show $ testRegexp "if_if=\"key\\\"word\"" r7 [(1, "if_if"), (2, "key\\\"word")] True
  putStrLn . show $ testRegexp "ifcasewhile=\"\\" r7 [(1, "ifcasewhile"), (2, "\\")] False
  putStrLn . show $ testRegexp "aaaaaaaaaaaaab" ((capture 1 $ kleene $ match1 'a') +*+ seque "aab") [(1, "aaaaaaaaaaa")] True
{-# LANGUAGE DeriveFunctor #-}

import Control.Applicative ((<$>), (<*>))
import Data.Char
import Text.ParserCombinators.ReadP

data NonEmptyList a = NonEmptyList a [a]
  deriving (Functor, Show, Eq)

many1' :: ReadP a -> ReadP (NonEmptyList a)
many1' = fmap toNonEmptyList . many1
  where toNonEmptyList (x:xs) = NonEmptyList x xs
        -- never happens because of many1:
        toNonEmptyList _ = undefined

(...) :: ReadP a -> ReadP b -> ReadP (a, b)
a ... b = (,) <$> a <*> b

searchForXinY :: ReadP ()
searchForXinY = (munch1 (/= '\n') ... skipSpaces) >> return ()

findCount :: ReadP Int
findCount = do
  _ <- string "findCount="
  sx <- munch1 isDigit
  _ <- skipSpaces
  return $ read sx -- can't fail: we only took digits

data PageRect = PageRect Int (Int, Int, Int, Int)
  deriving (Show, Eq)

pageRect :: ReadP PageRect
pageRect = do
    _ <- string "pages[" ... munch1 isDigit ... string "]="
    sp <- _int
    _ <- string " rects[i]=("
    sx <- _int
    _ <- string ", "
    sy <- _int
    _ <- string ", "
    sw <- _int
    _ <- string ", "
    sh <- _int
    _ <- string ")" ... skipSpaces
    return $ PageRect sp (sx, sy, sw, sh)
  where _int :: ReadP Int
        _int = fmap read $ munch1 isDigit

oneBlock :: ReadP (NonEmptyList PageRect)
oneBlock = findCount >> many1' pageRect

text = "morbi mattis in scripts/loremipsum.pdf\n\
       \findCount=1\n\
       \pages[0]=1 rects[i]=(287, 250, 62, 14)\n\
       \findCount=2\n\
       \pages[0]=1 rects[i]=(488, 609, 31, 14)\n\
       \pages[1]=2 rects[i]=(56, 57, 30, 14)\n\
       \total=2\n\
       \Time: 1560.82ms\n\
       \\n\
       \\n\
       \Backward:\n\
       \findCount=1\n\
       \pages[0]=1 rects[i]=(488, 609, 31, 14)\n\
       \pages[1]=2 rects[i]=(56, 57, 30, 14)\n\
       \findCount=2\n\
       \pages[0]=1 rects[i]=(287, 250, 62, 14)\n\
       \total=2\n\
       \Time: 1513.24ms\n"

fullParser = do
  searchForXinY
  forward <- many oneBlock
  _ <- string "total=" ... munch1 isDigit ... skipSpaces
  _ <- string "Time: " ... munch1 (\x -> isDigit x || x == '.') ... string "ms" ... skipSpaces
  _ <- string "Backward:" ... skipSpaces
  backward <- many oneBlock
  _ <- string "total=" ... munch1 isDigit ... skipSpaces
  _ <- string "Time: " ... munch1 (\x -> isDigit x || x == '.') ... string "ms" ... skipSpaces
  return (forward, backward)

main :: IO ()
main = putStrLn . show . readP_to_S fullParser $ text


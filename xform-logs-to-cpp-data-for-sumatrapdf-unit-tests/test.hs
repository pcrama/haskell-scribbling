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
searchForXinY = do
  _ <- string "Search for '"
  _ <- munch (not . (== '\''))
  _ <- string "' in '"
  _ <- munch (not . (== '\''))
  _ <- string "'\n"
  return ()

findCount :: ReadP Int
findCount = do
  _ <- string "findCount = "
  sx <- munch1 isDigit
  _ <- skipSpaces
  return $ read sx -- can't fail: we only took digits

data PageRect = PageRect Int (Int, Int, Int, Int)
  deriving (Show, Eq)

pageRect :: ReadP PageRect
pageRect = do
    _ <- string "- page[" ... munch1 isDigit ... string "]="
    sp <- _int
    _ <- string ", rect=("
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

text = "Search for 'xyz' in 'a.pdf'\nfindCount = 1\n- page[1]=12, rect=(1, 2, 3, 4)\n"

fullParser = searchForXinY ... oneBlock

main :: IO ()
main = putStrLn . show . readP_to_S fullParser $ text


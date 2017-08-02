{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}

import Control.Applicative ((<$>), (<*>))
import Control.Monad (forM_)
import Data.Char
import Data.List
import System.Directory
import System.IO
import Text.ParserCombinators.ReadP
import Text.Printf

data NonEmptyList a = NonEmptyList a [a]
  deriving (Functor, Show, Eq, Foldable)

many1' :: ReadP a -> ReadP (NonEmptyList a)
many1' = fmap toNonEmptyList . many1
  where toNonEmptyList (x:xs) = NonEmptyList x xs
        -- never happens because of many1:
        toNonEmptyList _ = undefined

(...) :: ReadP a -> ReadP b -> ReadP (a, b)
a ... b = (,) <$> a <*> b

searchForXinY :: ReadP String
searchForXinY = fmap fst $ munch1 (not . flip elem "\n\r") ... skipSpaces

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

-- struct RegressSearchInfo {
--     WCHAR *searchPhrase;
--     int count; // how many times it is found in the test document
--     int rectCounts[]; // how many rects or pages (see next struct members) are needed to locate each hit [count elements]
--     RectI rects[]; // rectangles covering the hits [sum(rectCounts) elements]
--     int pages[]; // pages on which the hist are found [sum(rectCounts) elements
-- };
--
-- Use it like this:
-- expected =
-- /*
-- struct TextSel {
-- int len;
-- int *pages;
-- RectI *rects;
-- };
--
-- int lili[] = { 1                   , 2                   , 2 };
-- RectI riri[] = { { 293, 126, 58, 14 },{ 335, 130, 59, 14 },{ 56, 671, 59, 14 } };
--
-- const TextSel expected[] = {
-- { 1, lili, riri },
-- { 1, &lili[1], &riri[1] },
-- { 1, &lili[2], &riri[2] },
-- { 0, nullptr, nullptr } // sentinel object to close the list
-- };
-- */

formatUsage :: SearchLog -> String
formatUsage (SearchLog s xs) =
  printf "const TextSel expected[%d] = { // %s\n\
         \  %s\
         \  { 0, nullptr, nullptr }\n\
         \};\n"
         (1 + len xs)
         s
         formatTextSel xs

formatTextSel :: [NonEmptyList PageRect] -> String
formatTextSel = intercalateAndBreak ", " 80 . snd . go (0, [])
  where go (count, result) xs =
    printf "{ %d, &(data.pages
formatRect :: PageRect -> String
formatRect (PageRect _ (x, y, w, h)) = printf "{%d, %d, %d, %d}" x y w h

-- foldMap :: (a -> m) -> t a -> m

formatFullStruct :: SearchLog -> String
formatFullStruct (SearchLog s xs) =
                     printf "const RegressSearchInfo data_ = {// %s\n\
                             \{ L\"%s\", // searchPhrase\n\
                             \  %d, // count\n\
                             \  { %s }, // rectCounts\n\
                             \  { %s }, // rects\n\
                             \  { %s } }; // pages\n"
                             s
                             s
                             (length xs)
                             (intercalateAndBreak ", " 80
                                        $ map (show . length) xs)
                             (intercalateAndBreak ", " 80
                                        $ foldMap (foldr ((:) . formatRect) []) xs)
                             (intercalateAndBreak ", " 80
                                        $ foldMap (foldr ((:) . (show . getPage)) []) xs)
  where getPage (PageRect p _) = p

intercalateAndBreak :: String -> Int -> [String] -> String
intercalateAndBreak sep width = go 0
  where length_sep = length sep
        go _ [] = []
        go _ [x] = x
        go w (x:xs) = let extra = length x + length_sep
                      in if w + extra >= width
                         then "\n  " ++ x ++ sep ++ go (extra + 2) xs
                         else x ++ sep ++ go (w + extra) xs
  -- where maybeBreak (soFar, w) next | w + length next > width = (soFar ++ sep ++ "\n    " ++ next, 4 + length next)
  --                                  | otherwise = (soFar ++ sep ++ next, w + length sep + length next)

fullParser = do
  searchString <- searchForXinY
  forward <- many oneBlock
  _ <- string "total=" ... munch1 isDigit ... skipSpaces
  _ <- string "Time: " ... munch1 (\x -> isDigit x || x == '.') ... string "ms" ... skipSpaces
  _ <- string "Backward:" ... skipSpaces
  backward <- many oneBlock
  _ <- string "total=" ... munch1 isDigit ... skipSpaces
  _ <- string "Time: " ... munch1 (\x -> isDigit x || x == '.') ... string "ms" ... skipSpaces
  return ((searchString, forward), backward)

data SearchLog = SearchLog String [NonEmptyList PageRect]

parseLogString :: String -> SearchLog
parseLogString = uncurry SearchLog . fst . fst . head . readP_to_S fullParser

workWithFile :: FilePath -> IO ()
workWithFile f = withFile f ReadMode $ \handle -> do
    xs <- hGetContents handle
    putStrLn . formatFullStruct . parseLogString $ xs

isLogFile :: FilePath -> Bool
isLogFile ('o':'u':'t':'-':xs) = True
isLogFile _ = False

main :: IO ()
main = do
    dirContent <- listDirectory "."
    forM_ [x | x <- dirContent, isLogFile x] workWithFile

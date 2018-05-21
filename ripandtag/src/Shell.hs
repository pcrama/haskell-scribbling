module Shell
  ( safeTrackName
  , breakOnPredicate -- for testing purposes
  )

where

import Data.Char (toUpper)
import Data.List (intercalate)

import ParseRipSpec (TrackRipSpec(..))

simplifyLatin1 :: [String]
simplifyLatin1 = ["e\xe9", "E\xc9"]

makeSafeChar :: Char -> Maybe Char
makeSafeChar x
  | x `elem` (['0'..'9'] ++ ".-_" ++ ['a'..'z'] ++ ['A'..'Z']) = Just x
  | otherwise = case filter (x `elem`) simplifyLatin1 of
                  (y:_):_ -> Just y
                  _ -> Nothing

breakOnPredicate :: (a -> Bool) -> [a] -> [[a]]
breakOnPredicate _ [] = []
breakOnPredicate p (z:zs) = go (p z) [z] zs []
  where --go :: Bool -> [a] -> [a] -> [[a]] -> [[a]]
        go _ [] [] r = reverse r
        go _ w [] r = reverse r ++ [reverse w]
        go lastP w (s:ss) r
           | lastP == p s = go lastP (s:w) ss r
           | otherwise = go (not lastP) [s] ss $ (reverse w):r

makeSafe :: String -> String
makeSafe x =
    let safeChars = map makeSafeChar x :: [Maybe Char]
        parts = breakOnPredicate isJust safeChars :: [[Maybe Char]]
    in concatMap (upCaseFirstChar . map (maybe '!' id))
               $ filter startWithJust parts
  where upCaseFirstChar (firstLetter:xs) = toUpper firstLetter:xs
        upCaseFirstChar xs = xs
        isJust Nothing = False
        isJust (Just _) = True
        startWithJust [] = False
        startWithJust (Nothing:_) = False
        startWithJust (Just _:_) = True

safeTrackName :: TrackRipSpec -> String
safeTrackName x = makeSafe
                $ intercalate "-"
                $ map (maybe "" id)
                $ filter (maybe False $ const True)
                $ [ artist x
                  , album x
                  , title x ]

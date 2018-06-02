module Shell
  ( safeTrackName
  , breakOnPredicate -- for testing purposes
  )

where

import Data.Char (toUpper)
import Data.List (intercalate)

import ParseRipSpec
  ( IntOrFollowing (..)
  , TrackRipSpec(..)
  )

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

-- Track name:
-- artist-album-track number-title
safeTrackName :: TrackRipSpec -> String
safeTrackName x = makeSafe
                $ intercalate "-"
                $ map (maybe "" id)
                $ filter (maybe False $ const True)
                $ [ artist x
                  , album x
                  , formatTrackNumber (track x) (total x)
                  , title x ]
  where formatTrackNumber :: IntOrFollowing -> Maybe Int -> Maybe String
        formatTrackNumber PrevPlusOne _ = Nothing -- should never happen
        formatTrackNumber (TheInt trk) Nothing = formatWidth trk 2
        formatTrackNumber (TheInt trk) (Just tot)
          | tot < 10 = formatWidth trk 1
          | tot < 100 = formatWidth trk 2
          | otherwise = formatWidth trk 3
        formatWidth n w =
          let s = show n
              l = length s
          in Just $ if l < w then replicate (w - l) '0' ++ s else s

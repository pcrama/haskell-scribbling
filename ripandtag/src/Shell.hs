{-# LANGUAGE RankNTypes #-}
module Shell
  ( safeTrackName
  , breakOnPredicate -- for testing purposes
  , shellCommands
  )

where

import Data.Char (toUpper)
import Data.List (intercalate)

import ParseRipSpec
  ( TrackRipSpec'(..)
  , PreciseTrackRipSpec
  )

simplifyLatin1 :: [String]
simplifyLatin1 = [ "a\xe4\xe0\xe1"
                 , "A\xc4\xc0\xc1"
                 , "c\xe7"
                 , "C\xc7"
                 , "e\xeb\xe9\xe8"
                 , "E\xcb\xc9\xc8"
                 , "i\xef\xed\xec"
                 , "I\xcf\xcd\xcc"
                 , "n\xf1"
                 , "N\xd1"
                 , "o\xf6\xf3\xf2"
                 , "O\xd6\xd3\xd2"
                 , "u\xfc\xfa\xf9"
                 , "U\xdc\xda\xd9"
                 , "y\xff\xfd"
                 , "Y\xdd" ]

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
safeTrackName :: PreciseTrackRipSpec -> String
safeTrackName x = makeSafe
                $ intercalate "-"
                $ map (maybe "" id)
                $ filter (maybe False $ const True)
                $ [ artist x
                  , album x
                  , formatTrackNumber (track x) (total x)
                  , title x ]
  where formatTrackNumber :: Int -> Maybe Int -> Maybe String
        formatTrackNumber trk Nothing = formatWidth trk 2
        formatTrackNumber trk (Just tot)
          | tot < 10 = formatWidth trk 1
          | tot < 100 = formatWidth trk 2
          | otherwise = formatWidth trk 3
        formatWidth n w =
          let s = show n
              l = length s
          in Just $ if l < w then replicate (w - l) '0' ++ s else s

shellCommands :: [PreciseTrackRipSpec] -> [String]
shellCommands = map shellCommand
  where shellCommand :: PreciseTrackRipSpec -> String
        shellCommand trs =
          let trackStr = show $ track trs
          in "icedax -vall cddb=0 speed=4 -D /dev/sr0 --track "
             ++ trackStr
             ++ " - | " -- write audio samples to stdout, then pipe to ...
             ++ "lame --preset 128 --add-id3v2 --id3v2-latin1"
             ++ (mkAccStringField title "tt"
               . mkAccStringField artist "ta"
               . mkAccStringField album "tl"
               . mkAccStringField genre "tg"
               . mkAccIntField year "ty"
               $ " --tn "
                 ++ case total trs of
                      Nothing -> trackStr
                      Just t -> trackStr ++ "/" ++ show t
                 ++ " - "       -- read from stdin
                 ++ safeTrackName trs
                 ++ ".mp3")
          where mkAccStringField :: (PreciseTrackRipSpec -> Maybe String)
                                 -> String
                                 -> (String -> String)
                mkAccStringField field name tl =
                  case field trs of
                    Nothing -> tl
                    (Just s) -> " --" ++ name ++ " " ++ posixQuote s ++ tl
                mkAccIntField :: (PreciseTrackRipSpec -> Maybe Int)
                              -> String
                              -> (String -> String)
                mkAccIntField field name tl =
                  case field trs of
                    Nothing -> tl
                    (Just i) -> " --" ++ name ++ " " ++ show i ++ tl
                posixQuote x = (  "'"
                               -- replace each single quote by a closing
                               -- single quote, an escaped single quote
                               -- and an opening single quote.  All other
                               -- characters need no quoting.
                               ++ concatMap (\c -> case c of
                                                '\'' -> "'\\''"
                                                _ -> c:"")
                                            x
                               ++ "'")

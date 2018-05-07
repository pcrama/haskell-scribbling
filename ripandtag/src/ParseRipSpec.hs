module ParseRipSpec
    ( TrackRipSpec(..)
    , IntOrFollowing(..)
    ) where

import Data.Char (isAlpha, toLower, digitToInt)
import Data.Function (on)
import Data.List (minimumBy, foldl')
import Data.List.NonEmpty (NonEmpty(..))
import Text.ParserCombinators.ReadP

data Key = Title | Artist | Track | Total | Album | Genre
  deriving (Show, Eq, Enum, Bounded)

data IntOrFollowing = PrevPlusOne | TheInt Int
  deriving (Show, Eq)

data TrackRipSpec = TRS
  { title :: Maybe String
  , artist :: Maybe String
  , track :: IntOrFollowing
  , total :: Maybe Int
  , album :: Maybe String
  , genre :: Maybe String
  } deriving (Show, Eq)

data CDRipSpec = CRS
  { info :: NonEmpty (Key, String)
  , overrides :: [CDRipSpec]
  } deriving (Show, Eq)

setFromKey :: Key -> String -> TrackRipSpec -> TrackRipSpec
setFromKey Title s trs = trs { title = Just s }
setFromKey Artist s trs = trs { artist = Just s }
setFromKey Album s trs = trs { album = Just s }
setFromKey Track s trs = trs { track = parseIntOrFollowing s }
setFromKey Total s trs = trs { total = parseInt s }
setFromKey Genre s trs = trs { genre = Just s }

emptyTrackRipSpec = TRS { title = Nothing
                        , artist = Nothing
                        , track = PrevPlusOne
                        , total = Nothing
                        , album = Nothing
                        , genre = Nothing }

expandOverrides :: TrackRipSpec -> CDRipSpec -> [TrackRipSpec]
expandOverrides base
                (CRS { info = (k, s) :| kkss
                     , overrides = overrides }) =
  let expBase = foldr (uncurry setFromKey) base $ (k, s):kkss
  in case overrides of
       [] -> [expBase]
       xs -> concatMap (expandOverrides expBase) xs

parseKeyString :: ReadP (Key, String)
parseKeyString = do
  keyName <- munch1 isAlpha
  skipSpaces
  _ <- char ':'
  skipSpaces
  value <- munch1 (not . (`elem` "\r\n"))
  case lookup (map toLower keyName)
              [ ("title", Title)
              , ("artist", Artist)
              , ("track", Track)
              , ("total", Total)
              , ("album", Album)
              , ("genre", Genre) ] of
    Just key -> return (key, value)
    Nothing -> pfail

eol1 = munch1 (`elem` "\r\n")

parseCRS :: Int -> ReadP CDRipSpec
parseCRS indent = do
  _ <- string $ replicate indent ' '
  _ <- char '-'
  nextIndent <- fmap (((1 + indent) +) . length) $ munch1 (== ' ')
  (kv:keyValueTail) <- sepBy1 parseKeyString
                            $ eol1 >> (string $ replicate nextIndent ' ')
  let info = kv :| keyValueTail
  overrides <- option [] $ do
    _ <- eol1
    sepBy (parseCRS nextIndent) eol1
  return $ CRS { info=info, overrides=overrides }

parseSpec :: ReadP [CDRipSpec]
parseSpec = sepBy (parseCRS 0) eol1 <* (munch (`elem` " \r\n") >> eof)

formatKeyValue (key, val) = show key ++ ": " ++ val

formatSpec = go1 0
  where go1 prefix = foldr (\crs tl -> go prefix crs ++ ('\n':tl)) ""
        go prefix (CRS {info=kv:|kvTail, overrides=overrides}) =
          (replicate prefix ' ') ++ "- " ++ formatKeyValue kv
          ++ (concatMap (('\n':(replicate (prefix + 2) ' ')) ++)
                      $ map formatKeyValue kvTail)
          ++ (concatMap ('\n':) $ map (go $ prefix + 2) overrides)

reformatSpec s =
  case readP_to_S parseSpec s of
    [(x, "")] -> formatSpec x
    [(x, y)] -> "Parse error in " ++ show y ++ " after\n" ++ formatSpec x
    lst@(_:_) -> "Parse error followed by " ++ (
      show $ minimumBy (compare `on` length) lst)
    _ -> "Unknown parse error"

parseTrackRipSpec :: String -> Maybe [TrackRipSpec]
parseTrackRipSpec s =
  case readP_to_S parseSpec s of
    [(x, "")] -> Just $ translateSpec x
    _ -> Nothing

translateSpec :: [CDRipSpec] -> [TrackRipSpec]
translateSpec = concatMap $ expandOverrides emptyTrackRipSpec

parseInt :: String -> Maybe Int
parseInt s =
    case readP_to_S (munch1 $ flip elem "0123456789") s of
      [(x, "")] -> Just $ s2i x
      _ -> Nothing
  where s2i = foldl' (\acc c -> 10 * acc + digitToInt c) 0

parseIntOrFollowing :: String -> IntOrFollowing
parseIntOrFollowing = maybe PrevPlusOne TheInt . parseInt

module ParseRipSpec
    ( TrackRipSpec(..)
    , IntOrFollowing(..)
    , CDRipSpec(..)
    , parseCRS
    , Key(..)
    , translateSpec
    , emptyTrackRipSpec
    , expandOverrides
    ) where

import Data.Char (isAlpha, toLower, digitToInt)
import Data.Function (on)
import Data.List (minimumBy, foldl', dropWhileEnd)
import Data.List.NonEmpty (NonEmpty(..))
import Text.ParserCombinators.ReadP

import Test.QuickCheck
  ( Arbitrary(..)
  , Gen
  , choose
  , elements
  , resize
  , sized
  , suchThat
  )

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

instance Arbitrary TrackRipSpec where
  arbitrary = do
      tit <- arbString 9
      art <- arbString 9
      tra <- fmap TheInt $ arbitrary `suchThat` (0 <)
      tot <- arbTotal
      alb <- arbString 5
      gen <- arbString 2
      return $ TRS { title = tit
                   , artist = art
                   , track = tra
                   , total = tot
                   , album = alb
                   , genre = gen
                   }
    where -- arbString :: Int -> Gen (Maybe String)
          arbString p = do
            -- 1 x Nothing, p x Just String
            pickJust <- elements $ False:replicate p True
            if pickJust
            then fmap Just $ (arbitrary `suchThat` ((1 <) . length) :: Gen String)
            else return Nothing
          -- arbTotal :: Gen (Maybe Int)
          arbTotal = do
            -- 10% Nothing, 90% Just Int
            pickJust <- elements $ False:replicate 9 True
            if pickJust
            then fmap Just $ arbitrary `suchThat` (\x -> (0 < x) && (x < 1000))
            else return Nothing

data CDRipSpec = CRS
  { info :: NonEmpty (Key, String)
  , overrides :: [CDRipSpec]
  } deriving (Show, Eq)

instance Arbitrary CDRipSpec where
  arbitrary = sized $ \size ->
    case size of
      0 -> do
             k <- elements [Title, Artist, Album, Track, Total, Genre]
             v <- elements ["ab", "cd", "efg", "hi", "jk", "l", "mn"
                           , "op", "qrs", "tuvw", "xy", "z a", "bcde"
                           , "fghi", "jklm", "n op", "Qr s", "TUV"
                           , "xYz"]
             return CRS { info = (k, v) :| [], overrides = [] }
      _ -> do
        let localMaxSize = 20
        lftSize <- choose (1, min localMaxSize size)
        let rgtSize = min localMaxSize $ max 0 $ size - lftSize - 1
        let f (CRS { info = (k, v) :| _ }) = (k, v)
        kvs <- mapM (fmap f . resize 0) $ replicate lftSize arbitrary
        overs <- mapM (resize $ min 3 $ rgtSize `div` 2)
                    $ replicate rgtSize arbitrary
        return $ CRS { info = head kvs :| tail kvs
                     , overrides = overs }
  shrink input@(CRS { info = hd :| tl, overrides = overs }) =
             case (null tl, null overs) of
               (True, True) -> []
               (True, False) -> [shrunkOvers]
               (False, True) -> [shrunkTail]
               (False, False) -> [shrunkBoth, shrunkOvers, shrunkTail]
           where shrunkOvers = input { overrides = [] }
                 shrunkTail = input { info = hd :| [] }
                 shrunkBoth = CRS { info = hd :| [], overrides = [] }

setFromKey :: TrackRipSpec -> (Key, String) -> TrackRipSpec
setFromKey trs (Title, s) = trs { title = Just s }
setFromKey trs (Artist, s) = trs { artist = Just s }
setFromKey trs (Album, s) = trs { album = Just s }
setFromKey trs (Track, s) = trs { track = parseIntOrFollowing s }
setFromKey trs (Total, s) = trs { total = parseInt s }
setFromKey trs (Genre, s) = trs { genre = Just s }

emptyTrackRipSpec :: TrackRipSpec
emptyTrackRipSpec = TRS { title = Nothing
                        , artist = Nothing
                        , track = PrevPlusOne
                        , total = Nothing
                        , album = Nothing
                        , genre = Nothing }

expandOverrides :: TrackRipSpec -> CDRipSpec -> [TrackRipSpec]
expandOverrides base
                (CRS { info = (k, s) :| kkss
                     , overrides = os  }) =
  -- foldl', not foldr so that the last (k, s) can override previous values
  let expBase = foldl' setFromKey base $ (k, s):kkss
  in case os of
       [] -> [expBase]
       _ -> concatMap (expandOverrides expBase) os

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
    Just key -> return (key, dropWhileEnd (`elem` " \t") value)
    Nothing -> pfail

eol1 :: ReadP String
eol1 = munch1 (`elem` "\r\n")

parseCRS :: Int -> ReadP CDRipSpec
parseCRS indent = do
  _ <- string $ replicate indent ' '
  _ <- char '-'
  nextIndent <- fmap (((1 + indent) +) . length) $ munch1 (== ' ')
  (kv:keyValueTail) <- sepBy1 parseKeyString
                            $ eol1 >> (string $ replicate nextIndent ' ')
  os <- option [] $ do
    _ <- eol1
    sepBy (parseCRS nextIndent) eol1
  return $ CRS { info=kv :| keyValueTail
               , overrides=os }

parseSpec :: ReadP [CDRipSpec]
parseSpec = sepBy (parseCRS 0) eol1 <* (munch (`elem` " \r\n") >> eof)

formatKeyValue :: Show a => (a, String) -> String
formatKeyValue (key, val) = show key ++ ": " ++ val

formatSpec :: [CDRipSpec] -> String
formatSpec = go1 0
  where go1 prefix = foldr (\crs tl -> go prefix crs ++ ('\n':tl)) ""
        go prefix (CRS {info=kv:|kvTail, overrides=os}) =
          (replicate prefix ' ') ++ "- " ++ formatKeyValue kv
          ++ (concatMap (('\n':(replicate (prefix + 2) ' ')) ++)
                      $ map formatKeyValue kvTail)
          ++ (concatMap ('\n':) $ map (go $ prefix + 2) os)

reformatSpec :: String -> String
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

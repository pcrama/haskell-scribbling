module Sha256FileCompare
    ( DuplicateFile(..)
    , ChangedFile(..)
    , RemovedFile(..)
    , OldAndNewFile(..)
    , NewFile(..)
    , FileEntry(..)
    , Hash(..)
    , parseChecksumLine
    , normalizeFileName
    , findOldAndNewSame
    , findDuplicates
    , findChanged
    , findRemoved
    , findNew
    ) where

import Data.Char (isSpace, toLower)
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Monoid (First(..))
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import System.FilePath (FilePath, (</>), takeDirectory, takeFileName)

newtype Hash = Hash String
  deriving (Show, Ord, Eq)

data DuplicateFile = DuplicateFile
    { dfHash         :: Hash
    , file1          :: FilePath
    , file2          :: FilePath
    , moreDuplicates :: [FilePath]
    } deriving (Show)

instance Eq DuplicateFile where
  DuplicateFile { dfHash=h1, file1=a1, file2=a2, moreDuplicates=a3 } == DuplicateFile { dfHash=h2, file1=b1, file2=b2, moreDuplicates=b3 } = h1 == h2 && sort (a1:a2:a3) == sort (b1:b2:b3)

data OldAndNewFile = OldAndNewFile
    { oanfHash :: Hash
    , oldFiles :: NonEmpty FilePath
    , newFiles :: NonEmpty FilePath
    } deriving (Show)

instance Eq OldAndNewFile where
  OldAndNewFile { oanfHash=h1, oldFiles=a1:|a1s, newFiles=a2:|a2s } == OldAndNewFile { oanfHash=h2, oldFiles=b1:|b1s, newFiles=b2:|b2s } = h1 == h2 && sort (a1:a1s) == sort (b1:b1s) && sort (a2:a2s) == sort (b2:b2s)

data ChangedFile = ChangedFile
    { oldHash :: Hash
    , oldFile :: FilePath
    , newHash :: Hash
    , newFile :: FilePath
    } deriving (Eq, Show)

data RemovedFile = RemovedFile
    { rfHash :: Hash
    , rfFile :: FilePath
    } deriving (Eq, Show)

data NewFile = NewFile
    { nfHash :: Hash
    , nfFile :: FilePath
    } deriving (Eq, Show)

data FileEntry = FileEntry
    { feHash :: Hash
    , feFile :: FilePath
    } deriving (Eq, Show)

parseChecksumLine :: String -> Maybe (Hash, FilePath)
parseChecksumLine line =
    let (hash, restOfLine) = span (\c -> ('0' <= c && c <= '9') || ('a' <= c && c <= 'f'))
                                  line
     in case restOfLine of
      ' ':' ':fName -> Just (Hash hash, fName)
      _ -> Nothing

normalizeFilePath :: Int -> FilePath -> String
normalizeFilePath level fp
  | level < 1 = normalizeFileName fp
  | otherwise = let directory = takeDirectory fp in normalizeFilePath (level - 1) directory </> normalizeFileName fp

normalizeFileName :: FilePath -> String
normalizeFileName =
    map toLower
    . filter (not . isSpace)
    . takeFileName

groupBy :: Ord k => (a -> k) -> (a -> v) -> [a] -> Map.Map k (NonEmpty v)
groupBy getKey getVal = Map.fromListWith (<>) . map (\a -> (getKey a, getVal a:|[]))

findDuplicates :: [FileEntry] -> [DuplicateFile]
findDuplicates entries =
    [ DuplicateFile { dfHash = hash
                    , file1 = f1
                    , file2 = f2
                    , moreDuplicates = rest
                    }
    | (hash, f1:|f2:rest) <- Map.toList grouped
    ]
  where
    grouped = groupBy feHash feFile entries

findOldAndNewSame :: [FileEntry] -> [FileEntry] -> [Either FileEntry OldAndNewFile]
findOldAndNewSame oldEntries newEntries =
    concatMap linkToOldOrReject $ Map.toList newGrouped
  where
    group = groupBy feHash feFile
    oldGrouped = group oldEntries
    newGrouped = group newEntries
    linkToOldOrReject (hash, newies) = case Map.lookup hash oldGrouped of
      Just oldies -> [Right $ OldAndNewFile { oanfHash = hash, oldFiles = oldies, newFiles = newies }]
      Nothing -> let hdNew:|tlNew = newies in map (Left . FileEntry hash) (hdNew:tlNew)

firstJust (a -> Maybe b) -> [a] -> Maybe b
firstJust f = foldr (\a b -> getFirst $ (First $ f a) <> First b) Nothing

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither new (Just cf) = Right cf
maybeToEither new Nothing = Left new

findChanged :: [FileEntry] -> [FileEntry] -> [Either FileEntry ChangedFile]
findChanged oldEntries newEntries =
    concatMap linkToSameNameOrReject $ Map.toList newGrouped
    -- [ ChangedFile
    --     { oldHash = feHash old
    --     , oldFile = feFile old
    --     , newHash = feHash new
    --     , newFile = feFile new
    --     }
  where
    grouped = groupBy (normalizeFileName . feFile) id
    oldGrouped = grouped oldEntries
    newGrouped = grouped newEntries
    linkToSameNameOrReject (_, hdNew:|tlNew) = map (\new -> maybeToEither new $ byLongestFilePath new) $ hdNew:tlNew
    byLongestFilePath new = do
      oldies <- Map.lookup (normalizeFileName $ feFile new) oldGrouped
      getFirst $ mconcat $ map (First . byFilePathUpTo new oldies) [3, 2, 1, 0]
    byFilePathUpTo new oldies level = do
      let normalizedNew = normalizeFilePath level $ feFile new
      sameOldAfterNormalization <- firstJust (\old -> if (normalizeFilePath level $ feFile old) == normalizedNew
                                                        then Just old else Nothing)
                                             oldies
      return $ ChangedFile { oldHash = feHash sameOldAfterNormalization
                           , oldFile = feFile sameOldAfterNormalization
                           , newHash = feHash new
                           , newFile = feFile new}

findRemoved :: [FileEntry] -> [FileEntry] -> [Either FileEntry RemovedFile]
findRemoved oldEntries newEntries =
    [ RemovedFile
        { rfHash = feHash old
        , rfFile = feFile old
        }
    | old <- oldEntries
    , let key = normalizeFileName (feFile old)
    , not $ key `Set.member` changedOldNames
    , not $ key `Set.member` newNames
    ]
  where
    changedOldNames = Set.fromList $ map (normalizeFileName . oldFile) changed
    newNames = Set.fromList $ map (normalizeFileName . feFile) newEntries

findNew :: [FileEntry] -> [FileEntry] -> [ChangedFile] -> [RemovedFile] -> [NewFile]
findNew oldEntries newEntries changed removed =
    [ NewFile
        { nfHash = feHash new
        , nfFile = feFile new
        }
    | new <- newEntries
    , let key = normalizeFileName (feFile new)
    , key `notElem` changedNames
    , key `notElem` removedNames
    , key `notElem` oldNames
    ]
  where
    changedNames = map (normalizeFileName . newFile) changed
    removedNames = map (normalizeFileName . rfFile) removed
    oldNames = map (normalizeFileName . feFile) oldEntries

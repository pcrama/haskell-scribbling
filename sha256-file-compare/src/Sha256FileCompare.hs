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
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import System.FilePath (FilePath, takeFileName)

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

normalizeFileName :: FilePath -> String
normalizeFileName =
    map toLower
    . filter (not . isSpace)
    . takeFileName

findDuplicates :: [FileEntry] -> [DuplicateFile]
findDuplicates entries =
    [ DuplicateFile { dfHash = hash
                    , file1 = f1
                    , file2 = f2
                    , moreDuplicates = rest
                    }
    | (hash, f1:f2:rest) <- Map.toList grouped
    ]
  where
    grouped =
        Map.fromListWith (++)
            [ (feHash e, [feFile e]) | e <- entries ]

findOldAndNewSame :: [FileEntry] -> [FileEntry] -> [OldAndNewFile]
findOldAndNewSame oldEntries newEntries =
    [ OldAndNewFile { oanfHash = hash, oldFiles = oldies, newFiles = newies }
    | (hash, oldies) <- Map.toList oldGrouped
    , Just newies <- [Map.lookup hash newGrouped] ]
  where
    group entries = Map.fromListWith (<>)
                                     [ (feHash e, feFile e:|[])
                                     | e <- entries ]
    oldGrouped = group oldEntries
    newGrouped = group newEntries

findChanged :: [FileEntry] -> [FileEntry] -> [OldAndNewFile] -> [ChangedFile]
findChanged oldEntries newEntries oldAndNewFiles =
    [ ChangedFile
        { oldHash = feHash old
        , oldFile = feFile old
        , newHash = feHash new
        , newFile = feFile new
        }
    | old <- oldEntries
    , not $ feHash old `Set.member` oldAndNewSet
    , Just new <- [Map.lookup (normalizeFileName (feFile old)) newMap]
    , feHash old /= feHash new
    , not $ feHash new `Set.member` oldAndNewSet
    ]
  where
    oldAndNewSet = Set.fromList $ map oanfHash oldAndNewFiles
    newMap = Map.fromList $ map (\fe -> (normalizeFileName (feFile fe), fe)) newEntries

findRemoved :: [FileEntry] -> [FileEntry] -> [ChangedFile] -> [RemovedFile]
findRemoved oldEntries newEntries changed =
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

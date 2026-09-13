module Main where

import Data.List.NonEmpty(NonEmpty(..))
import Sha256FileCompare
import Test.HUnit

h1, h2, h3, h4 :: String
h1 = replicate 64 'a'
h2 = replicate 64 'b'
h3 = replicate 64 'c'
h4 = replicate 64 'd'

hash1, hash2, hash3, hash4 :: Hash
hash1 = Hash h1
hash2 = Hash h2
hash3 = Hash h3
hash4 = Hash h4

tests :: Test
tests = TestList
    [ "parse checksum line" ~:
        Just (hash1, "Photos/2026/photo.jpg")
        ~=? parseChecksumLine (h1 ++ "  Photos/2026/photo.jpg")
 
    , "parse checksum line with single spaces" ~:
        Just (hash1, "Photos /2026/photo 1.jpg")
        ~=? parseChecksumLine (h1 ++ "  Photos /2026/photo 1.jpg")

    , "parse checksum line with double spaces" ~:
        Just (hash1, "Photos /  2026/photo  1.jpg")
        ~=? parseChecksumLine (h1 ++ "  Photos /  2026/photo  1.jpg")

    , "parse empty line" ~:
        Nothing ~=? parseChecksumLine ""

    , "normalize basename, case and spaces" ~:
        "photo2026.jpg"
        ~=? normalizeFileName "Photos/My Dir/PHOTO 2026.jpg"

    , "normalize preserves non-space filename characters" ~:
        "foo_bar.jpg"
        ~=? normalizeFileName "A/Foo_Bar.jpg"

    , "3 duplicates grouped by hash" ~:
        [ DuplicateFile hash1 "A/one.jpg" "B/two.jpg" ["C/three.jpg"] ]
        ~=? findDuplicates
            [ FileEntry hash1 "A/one.jpg"
            , FileEntry hash1 "B/two.jpg"
            , FileEntry hash1 "C/three.jpg"
            , FileEntry hash2 "D/other.jpg"
            ]

    , "2 duplicates grouped by hash" ~:
        [ DuplicateFile hash1 "A/one.jpg" "B/two.jpg" [] ]
        ~=? findDuplicates
            [ FileEntry hash1 "A/one.jpg"
            , FileEntry hash2 "D/other.jpg"
            , FileEntry hash1 "B/two.jpg"
            ]

    , "same hashes in old and new" ~:
        [ OldAndNewFile hash1 ("A/one.jpg":|[]) ("B/two.jpg":|["C/222.jpg"]) ]
        ~=? findOldAndNewSame [ FileEntry hash1 "A/one.jpg", FileEntry hash2 "A/two.jpg", FileEntry hash3 "A/three.jpg" ]
                              [ FileEntry hash4 "B/one.jpg", FileEntry hash1 "B/two.jpg", FileEntry hash1 "C/222.jpg" ]

    , "different hashes with same normalized basename are changed" ~:
        [ ChangedFile hash1 "Old/Photo One.JPG" hash2 "New/photoone.jpg" ]
        ~=? findChanged
            [ FileEntry hash1 "Old/Photo One.JPG" ]
            [ FileEntry hash2 "New/photoone.jpg" ]
            []

    , "same hash with same normalized basename is not changed" ~:
        []
        ~=? findChanged
            [ FileEntry hash1 "Old/Photo One.JPG" ]
            [ FileEntry hash1 "New/photoone.jpg" ]
            []

    , "same hash with same normalized basename is not changed when resized photo in new" ~:
        []
        ~=? findChanged
            [ FileEntry hash1 "Old/Photo One.JPG" ]
            [ FileEntry hash1 "New/photoone.jpg", FileEntry hash2 "Resampled/photoone.jpg" ]
            [ OldAndNewFile hash1 ("Old/Photo One.JPG":|[]) ("New/photoone.jpg":|[])]

    , "same hash with same normalized basename is not changed when resized photo in old" ~:
        []
        ~=? findChanged
            [ FileEntry hash1 "Old/Photo One.JPG", FileEntry hash2 "Resampled/photoone.jpg" ]
            [ FileEntry hash1 "New/photoone.jpg" ]
            [ OldAndNewFile hash1 ("Old/Photo One.JPG":|[]) ("New/photoone.jpg":|[])]

    , "different directories still match" ~:
        [ ChangedFile hash1 "Old/x.jpg" hash2 "Jean/X.JPG" ]
        ~=? findChanged
            [ FileEntry hash1 "Old/x.jpg" ]
            [ FileEntry hash2 "Jean/X.JPG" ]
            []

    , "removed means absent from new and changed" ~:
        [ RemovedFile hash1 "Old/gone.jpg" ]
        ~=? findRemoved
            [ FileEntry hash1 "Old/gone.jpg"
            , FileEntry hash2 "Old/changed.jpg"
            ]
            [ FileEntry hash3 "New/changed.jpg" ]
            [ ChangedFile hash2 "Old/changed.jpg" hash3 "New/changed.jpg" ]

    , "same basename in new is not removed" ~:
        []
        ~=? findRemoved
            [ FileEntry hash1 "Old/photo.jpg" ]
            [ FileEntry hash2 "New/PHOTO.JPG" ]
            []

    , "new excludes changed and removed basenames" ~:
        [ NewFile hash4 "New/brand-new.jpg" ]
        ~=? findNew
            [ FileEntry hash1 "Old/changed.jpg"
            , FileEntry hash2 "Old/gone.jpg"
            ]
            [ FileEntry hash3 "New/changed.jpg"
            , FileEntry hash4 "New/brand-new.jpg"
            ]
            [ ChangedFile hash1 "Old/changed.jpg" hash3 "New/changed.jpg" ]
            [ RemovedFile hash2 "Old/gone.jpg" ]

    , "your example's unchanged file is ignored" ~:
        ([], [], [], [])
        ~=? let
            old = [FileEntry hash1 "Photos/2026/Souper italien/20260328-_DSC3736.jpg"]
            new = [FileEntry hash1 "2026/Souper italien /Jean/20260328-_DSC3736.jpg"]
            c = findChanged old new []
            r = findRemoved old new c
            n = findNew old new c r
            in (findDuplicates old, c, r, n)
    ]

main :: IO ()
main = do
    testCounts <- runTestTT tests
    if errors testCounts + failures testCounts == 0
        then putStrLn "All tests passed."
        else error "Tests failed."

module Main where

-- cabal run compare-files -- input-1-old.txt input-2-new.txt

import Sha256FileCompare
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [oldPath, newPath] -> do
            oldEntries <- readChecksumFile oldPath
            newEntries <- readChecksumFile newPath

            let duplicates = findDuplicates oldEntries
                Result old1 new1 synced = findOldAndNewSame oldEntries newEntries
                Result old2 new2 changed = findChanged old1 new1

            putStrLn "=== DUPLICATE FILES ==="
            mapM_ print duplicates
            putStrLn "\n=== CHANGED FILES ==="
            mapM_ print changed
            putStrLn "\n=== remaining old files ==="
            mapM_ print old2
            putStrLn "\n=== remaining new files ==="
            mapM_ print new2

        _ -> die "Usage: compare-files old.txt new.txt"

readChecksumFile :: FilePath -> IO [FileEntry]
readChecksumFile path = do
    contents <- readFile path
    pure
        [ FileEntry hash file
        | line <- lines contents
        , Just (hash, file) <- [parseChecksumLine line]
        ]

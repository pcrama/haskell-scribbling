module Main where

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
                synced     = findOldAndNewSame oldEntries newEntries
                changed    = findChanged oldEntries newEntries synced
                removed    = findRemoved oldEntries newEntries changed
                newFiles   = findNew oldEntries newEntries changed removed

            putStrLn "=== DUPLICATE FILES ==="
            mapM_ print duplicates
            putStrLn "\n=== CHANGED FILES ==="
            mapM_ print changed
            putStrLn "\n=== REMOVED FILES ==="
            mapM_ print removed
            putStrLn "\n=== NEW FILES ==="
            mapM_ print newFiles

        _ -> die "Usage: compare-files old.txt new.txt"

readChecksumFile :: FilePath -> IO [FileEntry]
readChecksumFile path = do
    contents <- readFile path
    pure
        [ FileEntry hash file
        | line <- lines contents
        , Just (hash, file) <- [parseChecksumLine line]
        ]

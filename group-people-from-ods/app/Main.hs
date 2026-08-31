module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import qualified Data.Text.IO as TIO
import MyLib (
  groups
  , odsByteString
  , printGroups
  , readPeople
  , readRows
  )

exit :: IO a
exit = exitWith ExitSuccess

parse :: [String] -> IO String
parse ("-h":_) = usage >> exit
parse [] = usage >> exit
parse (_:_:_) = usage >> exit
parse [fn] = return fn

usage :: IO ()
usage = putStrLn "cabal run exe:group-people-from-ods [-h | file-name.ods]"

main :: IO ()
main = do
  fn <- getArgs >>= parse
  people <- (readPeople . readRows <$>) <$> odsByteString fn
  case people of
    Left e -> TIO.putStrLn $ "Error: " <> e
    Right p -> printGroups $ groups p

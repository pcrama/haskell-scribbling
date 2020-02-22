module Main where

import qualified Data.Text as T

import Password

main :: IO ()
main = do
  let mbP = mkPassword . T.pack $ "Password"
  putStrLn $ "mbP=" ++ show mbP
  case mbP of
    Just p -> putStrLn ("SHA1Prefix = " ++ show (sha1Prefix p))
           >> putStrLn ("compareWithSha1 p \"8be3c943b1609fffbfc51aad666d0a04adf83c9d\" = "
                     ++ (show $ compareWithSha1 p "8be3c943b1609fffbfc51aad666d0a04adf83c9d"))
    Nothing -> putStrLn "Not a valid password"

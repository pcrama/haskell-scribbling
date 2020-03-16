module Main where

import           Data.Char (intToDigit)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Client     (newManager, parseRequest, httpLbs, responseStatus, responseBody)
import           Network.HTTP.Client.TLS (tlsManagerSettings)

import Password

formatURL :: SHA1Prefix -> String
formatURL (SHA1Prefix x y z) = "https://api.pwnedpasswords.com/range/" <> s1 <> s2 <> (d:"")
  where toS a = (intToDigit $ fromIntegral a `div` 16):(intToDigit $ fromIntegral a `mod` 16):""
        s1 = toS x
        s2 = toS y
        (d:_) = toS z

main :: IO ()
main = do
  let mbP = mkPassword . T.pack $ "Password"
  putStrLn $ "mbP=" ++ show mbP
  case mbP of
    Just p -> do
      putStrLn ("SHA1Prefix = " ++ show (sha1Prefix p))
      putStrLn ("compareWithSha1 p \"8be3c943b1609fffbfc51aad666d0a04adf83c9d\" = "
             ++ (show $ compareWithSha1 p "8be3c943b1609fffbfc51aad666d0a04adf83c9d"))
      manager <- newManager tlsManagerSettings
      request <- parseRequest $ formatURL $ sha1Prefix p
      response <- httpLbs request manager
      putStrLn $ "The status code was: " ++ (show $ responseStatus response)
      L8.putStrLn $ responseBody $ response
    Nothing -> putStrLn "Not a valid password"

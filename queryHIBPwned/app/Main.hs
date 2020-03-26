{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import           Control.Monad           (forM_)
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Char               (intToDigit)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Word               (Word8)
import           Network.HTTP.Client     (
  httpLbs
  , newManager
  , parseRequest
  , responseStatus
  , responseBody)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.HTTP.Types.Status (statusCode)

import AppMonad
import Password

formatURL :: SHA1Prefix -> String
formatURL (SHA1Prefix x y z) = "https://api.pwnedpasswords.com/range/" <> s1 <> s2 <> (d:"")
  where toS a = (formatNibble $ a `div` 16):(formatNibble a):""
        s1 = toS x
        s2 = toS y
        (d:_) = toS z


parseBody :: L8.ByteString -> [(L8.ByteString, Int)]
parseBody response = map (fixSnd . L8.break (== ':')) $ L8.lines response
  where fixSnd (a, b) = (a
                        , maybe 0 fst $ do
                            s1 <- L8.stripPrefix ":" b
                            let s2 = maybe s1 id $ L8.stripSuffix "\r" s1
                            L8.readInt s2)


formatNibble :: Word8 -> Char
formatNibble x = intToDigit $ fromIntegral $ x `mod` 16


formatByte :: Word8 -> L8.ByteString
formatByte x = L8.pack [formatNibble $ x `div` 16, formatNibble $ x `mod` 16]


formatSHA1Prefix :: SHA1Prefix -> L8.ByteString
formatSHA1Prefix (SHA1Prefix a b c) =
       formatByte a
    <> formatByte b
    <> (L8.singleton $ formatNibble $ c `div` 16)


newtype QueryHibpIO a = QueryHibpIO (IO a)
  deriving (Functor, Applicative, Monad)


instance AppMonad [Char] QueryHibpIO where
  readNetrc f = fmap f $ QueryHibpIO $ TIO.getContents
  queryPassword s = do
      manager <- QueryHibpIO $ newManager tlsManagerSettings
      request <- QueryHibpIO $ parseRequest $ formatURL $ s
      response <- QueryHibpIO $ httpLbs request manager
      putLog $ "The status code was: " <> (show $ responseStatus response)
      let pwData = parseBody $ responseBody $ response
      return $ Right pwData
  apiKey = return Nothing
  queryUsername _ u = return $ Left $ "No API key available to query for " <> T.unpack u
  putLog = QueryHibpIO . putStrLn


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
      case statusCode $ responseStatus response of
        200 -> let pwData = parseBody $ responseBody $ response
                   sha1prefix = formatSHA1Prefix $ sha1Prefix p in
               forM_ pwData $ \(k, v) -> do
                 let fullSHA1 = sha1prefix <> k
                 case (compareWithSha1 p fullSHA1, v > 0) of
                   (True, True) -> putStrLn $ "Your password has been used "
                                           <> show v
                                           <> " time" <> (if v == 1 then "" else "s")
                   (True, False) -> putStrLn $ "Your password is in the list with a use count of 0.  Padding?"
                   (False, _) -> return ()
        c -> putStrLn $ "Error: " <> show c
    Nothing -> putStrLn "Not a valid password"

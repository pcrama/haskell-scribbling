{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

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


newtype QueryHibpIO a = QueryHibpIO { runQueryHibpIO :: (IO a) }
  deriving (Functor, Applicative, Monad)


instance AppMonad [Char] QueryHibpIO where
  readNetrc f = fmap f $ QueryHibpIO $ TIO.getContents
  queryPassword s = do
      manager <- QueryHibpIO $ newManager tlsManagerSettings
      request <- QueryHibpIO $ parseRequest $ formatURL $ s
      response <- QueryHibpIO $ httpLbs request manager
      let status = responseStatus response
      let sci = statusCode status
      if 200 <= sci && sci < 300
      then return $ Right $ parseBody $ responseBody response
      else return $ Left $ show status
  apiKey = return Nothing
  -- TODO: get an API key and implement user name querying, too.
  queryUsername _ u = return $ Left $ "No API key available to query for " <> T.unpack u
  putLog = QueryHibpIO . putStrLn


main :: IO ()
main = runQueryHibpIO queryHIBPwned

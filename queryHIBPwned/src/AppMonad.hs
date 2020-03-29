{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module AppMonad (
  AppMonad(..)
, passwordPopularity
, queryHIBPwned
)
where

import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Char (intToDigit)
import           Data.Either (either)
import           Data.List (intercalate, nub)
import           Data.Maybe (maybe)
import qualified Data.Text as T
import           Data.Traversable (traverse)
import           Data.Word (Word8)
import           Text.Megaparsec (runParser)

import Parser
import Password

-- | All operations necessary to query "Have I Been Pwned"
--   k: Type of the API key
--   m: base monad
class Monad m => AppMonad k m | m -> k where
  -- | Pass .netrc content to a parsing function and return its parsed
  --   representation.
  --
  --   NB: Maybe instead of Either to insist that code in AppMonad doesn't
  --   even have access to the passwords through parsing error messages.
  --
  --   In practice, a is ParsedEntry, but using a type variable means (by
  --   parametricity) that the output is either Nothing or really the
  --   parsing function's output.
  readNetrc :: (T.Text -> Maybe (Netrc a)) -- ^ parsing function
            -> m (Maybe (Netrc a)) -- ^ .netrc content
  -- | Send a password's SHA1 prefix to HIBPwned to query if the password
  --   is known in any clear text leak.
  queryPassword :: SHA1Prefix -- ^ SHA1 prefix
                -> m (Either String [(L.ByteString, Int)]) -- ^ list of SHA1 value suffixes and counts
  -- | Get API key (needed for user name queries to HIBP)
  apiKey :: m (Maybe k)
  -- | Send a user name to HIBPwned to check if the user name was in a
  --   leaked database.
  queryUsername :: k -- ^ API key
                -> T.Text -- ^ user name
                -> m (Either String [String]) -- ^ query result
  -- | Display log/error message
  putLog :: String -> m ()


passwordPopularity :: Password -> [(L.ByteString, Int)] -> Int
passwordPopularity p = sum . map snd . filter isMatchingPassword
  where formatNibble :: Word8 -> Char
        formatNibble x = intToDigit $ fromIntegral $ x `mod` 16
        formatByte :: Word8 -> L.ByteString
        formatByte x = L.pack [formatNibble $ x `div` 16, formatNibble $ x `mod` 16]
        formatSHA1Prefix :: SHA1Prefix -> L.ByteString
        formatSHA1Prefix (SHA1Prefix a b c) = formatByte a
                                           <> formatByte b
                                           <> (L.singleton $ formatNibble $ c `div` 16)
        prefix = formatSHA1Prefix $ sha1Prefix p
        isMatchingPassword (sha1, _) = compareWithSha1 p (prefix <> sha1)


queryHIBPwned :: AppMonad k m => m ()
queryHIBPwned = do
  mbParsedEntries <- readNetrc (either (const Nothing) Just . runParser netrcParser "input")
  case mbParsedEntries of
    Nothing -> putLog "Parse error"
    Just parsedEntries -> do
      let eiEntries = traverse validateParsedEntry parsedEntries -- :: Either [String] (Netrc Entry)
      case eiEntries of
        Left errors -> putLog $ intercalate "\n" errors
        Right entries -> do
          let netrcAsList = foldrNetrc (:) [] entries
          mapM_ (\z -> do
                   verifyPasswordPopularityIs0 (Just . password) "password" z
                   verifyPasswordPopularityIs0 account "account" z)
                netrcAsList
          mapM_ verifyUserAccountNotLeaked
              $ nub $ map (login . snd) netrcAsList


-- | Query HIBPwned for a password's SHA1 and verify the returned SHA1s
--   against the password
verifyPasswordPopularityIs0 :: AppMonad k m
                            => (Entry -> Maybe Password) -- ^ extract password/account from Entry
                            -> String -- ^ name of accessor above for logging purposes
                            -> (Maybe T.Text, Entry) -- ^ machine name + Entry
                            -> m ()
verifyPasswordPopularityIs0 accessor accessorName z@(mbMachine, entry) =
  case accessor entry of
    Nothing -> return ()
    Just pw -> do
      eiPasswordData <- queryPassword $ sha1Prefix pw
      case eiPasswordData of
        Left errorMsg -> putLog $ errorMsg <> " while querying HIBP about " <> (maybe "<default>" T.unpack mbMachine) <> " " <> accessorName <> "."
        Right pwMap ->
          case passwordPopularity pw pwMap of
            0 -> return ()
            popularity -> putLog $ "The " <> accessorName <> " of " <> show z <> " has been used " <> show popularity <> " times."


verifyUserAccountNotLeaked :: AppMonad k m
                           => T.Text -- ^ user name
                           -> m ()
verifyUserAccountNotLeaked user = do
  mbApiKey <- apiKey
  case mbApiKey of
    Nothing -> putLog $ "No API key available to query HIBP about user name '" <> T.unpack user <> "'."
    Just a -> do
      eiUserLeaked <- queryUsername a user
      case eiUserLeaked of
        Left errorMsg -> putLog $ errorMsg <> " while querying HIBP about " <> (T.unpack user) <> "."
        Right [] -> return ()
        Right x -> let sep = "\n  " in putLog $ (T.unpack user) <> ":" <> sep <> (intercalate sep x)

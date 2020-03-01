module AppMonad (
  AppMonad(..)
, queryHIBPwned
, verifyPasswordPopularityIs0
, verifyUserAccountNotLeaked
)
where

import           Data.Either (either)
import           Data.List (intercalate, nub)
import           Data.Maybe (maybe)
import qualified Data.Text as T
import           Data.Traversable (traverse)
import           Text.Megaparsec (runParser)

import Parser
import Password


class Monad m => AppMonad m where
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
                -> m (Either String [(String, Int)]) -- ^ list of SHA1 values and counts
  -- | Send a user name to HIBPwned to check if the user name was in a
  --   leaked database.
  queryUsername :: T.Text -- ^ user name
                -> m (Either String [String]) -- ^ query result
  -- | Display log/error message
  putLog :: String -> m ()


passwordPopularity :: Password -> [(String, Int)] -> Int
passwordPopularity p = sum . map snd . filter isMatchingPassword
  where isMatchingPassword (sha1, _) = compareWithSha1 p sha1


queryHIBPwned :: AppMonad m => m ()
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
verifyPasswordPopularityIs0 :: AppMonad m
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


verifyUserAccountNotLeaked :: AppMonad m
                           => T.Text -- ^ user name
                           -> m ()
verifyUserAccountNotLeaked user = do
  eiUserLeaked <- queryUsername user
  case eiUserLeaked of
    Left errorMsg -> putLog $ errorMsg <> " while querying HIBP about " <> (T.unpack user) <> "."
    Right [] -> return ()
    Right x -> let sep = "\n  " in putLog $ (T.unpack user) <> ":" <> sep <> (intercalate sep x)

module Parser
( Entry(..)
, Netrc(..)
, ParsedEntry(..)
, foldrNetrc
, netrcNamedEntry
, netrcParser
, validateParsedEntry
)
where

import           Control.Monad (void)
import qualified Data.Text as T
import           Data.Maybe (catMaybes)
import           Data.Void (Void)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

import Password (Password, isPasswordChar, mkPassword)


data Entry = Entry {
    scheme :: Maybe T.Text
  , login :: T.Text
  , account :: Maybe Password
  , password :: Password
} deriving (Show)


data ParsedEntry = ParsedEntry {
    parsedSchemes :: [T.Text]
  , parsedLogins :: [T.Text]
  , parsedPasswords :: [Password]
  , parsedAccounts :: [Password]
} deriving (Show)


-- | A .netrc file's content
--
-- Parameterize with ParsedEntry & Entry
data Netrc a = Netrc [(T.Text, a)] -- ^ named entry
                     (Maybe (a -- ^ default entry
                            , [(T.Text, a)] -- ^ named entries after default entry
                            ))
  deriving (Show, Functor, Foldable, Traversable)


foldrNetrc :: ((Maybe T.Text, a) -> b -> b) -> b -> Netrc a -> b
foldrNetrc f b0 (Netrc before mbRest) = foldr f' b1 before
  where b1 = case mbRest of
                  Just (def, after) -> f (Nothing, def) $ foldr f' b0 after
                  Nothing -> b0
        f' (machineName, pe) b = f (Just machineName, pe) b


type Parser = P.Parsec Void T.Text


-- To test in ghci on my tablet:
-- :module + Data.Void Data.Text Text.Megaparsec Text.Megaparsec.Char Text.Megaparsec.Char.Lexer
-- let netrcSpace = Text.Megaparsec.Char.Lexer.space space1 (skipLineComment $ pack "#") Text.Megaparsec.empty :: Parsec Void Text ()
-- let mkParser symbol name = Text.Megaparsec.Char.Lexer.symbol netrcSpace (pack symbol) *> takeWhile1P (Just $ name ++ " (non blank/control ASCII)") (\c -> ' ' < c && c < '\x7f') <* netrcSpace
-- let netrcMachine = mkParser "machine" "host name"
-- let netrcScheme = mkParser "scheme" "https/ssh/..."
-- runParser netrcMachine "ghci" (pack "machine \x81")


netrcSpace :: Parser ()
netrcSpace = L.space C.space1
                     (L.skipLineComment "#")
                     P.empty


mkParser :: T.Text -> String -> Parser T.Text
mkParser symbol name =
     L.symbol netrcSpace symbol
  *> P.takeWhile1P (Just $ name ++ " (non blank/control ASCII)")
                   isPasswordChar
  <* netrcSpace


netrcMachine :: Parser T.Text
netrcMachine = mkParser "machine" "host name"


netrcScheme :: Parser T.Text
netrcScheme = mkParser "scheme" "https/ssh/..."


netrcLogin :: Parser T.Text
netrcLogin = mkParser "login" "user name"


mkPasswordParser :: T.Text -> String -> Parser Password
mkPasswordParser symbol name = do
  txt <- mkParser symbol name
  case mkPassword txt of
    Just p -> return p
    Nothing -> fail $ "Could not parse " ++ name


netrcPassword :: Parser Password
netrcPassword = mkPasswordParser "password" "password"


netrcAccount :: Parser Password
netrcAccount = mkPasswordParser "account" "secondary secret"


netrcParser :: Parser (Netrc ParsedEntry)
netrcParser = do
  void $ netrcSpace
  beforeDefault <- P.many netrcNamedEntry
  mbDefaultEntry <- P.optional netrcDefaultEntry
  case mbDefaultEntry of
    Just defaultEntry -> do
      afterDefault <- P.many netrcNamedEntry
      void $ P.eof
      return $ Netrc beforeDefault $ Just (defaultEntry, afterDefault)
    Nothing -> do
      void $ P.eof
      return $ Netrc beforeDefault Nothing


netrcNamedEntry :: Parser (T.Text, ParsedEntry)
netrcNamedEntry = do
  machn <- netrcMachine
  details <- gatherEntryDetails 
  return (machn, details)


netrcDefaultEntry :: Parser ParsedEntry
netrcDefaultEntry = L.symbol netrcSpace "default" *> gatherEntryDetails


gatherEntryDetails :: Parser ParsedEntry
gatherEntryDetails = acc [] [] [] []
  where acc :: [T.Text] -> [T.Text] -> [Password] -> [Password] -> Parser ParsedEntry
        acc schemes logins passwords accounts = do
          mbS <- P.optional netrcScheme
          case mbS of
            Just s -> acc (schemes ++ [s]) logins passwords accounts
            Nothing -> do
              mbL <- P.optional netrcLogin
              case mbL of
                Just l -> acc schemes (logins ++ [l]) passwords accounts
                Nothing -> do
                  mbP <- P.optional netrcPassword
                  case mbP of
                    Just p -> acc schemes logins (passwords ++ [p]) accounts
                    Nothing -> do
                      mbA <- P.optional netrcAccount
                      case mbA of
                        Just a -> acc schemes logins passwords (accounts ++ [a])
                        Nothing -> return ParsedEntry {
                                              parsedSchemes = schemes
                                            , parsedLogins = logins
                                            , parsedPasswords = passwords
                                            , parsedAccounts = accounts
                                            }


validateParsedEntry :: ParsedEntry -> Either [String] Entry
validateParsedEntry (ParsedEntry {
    parsedSchemes = sc
  , parsedLogins = lo
  , parsedPasswords = pa
  , parsedAccounts = ac
  }) = case (validateSchemes, validateLogins, validatePasswords, validateAccounts) of
         (Right schm, Right lgn, Right psswrd, Right accnt) ->
           Right Entry { scheme = schm
                       , login = lgn
                       , password = psswrd
                       , account = accnt
                       }
         (ls, ll, lp, la) -> Left $ catMaybes [l2j ls, l2j ll, l2j lp, l2j la]
  where zeroOrOne _ [] = Right Nothing
        zeroOrOne _ [x] = Right $ Just x
        zeroOrOne s _ = Left $ "0 or 1 of " ++ s
        exactlyOne s [] = Left $ s ++ " is mandatory"
        exactlyOne _ [x] = Right x
        exactlyOne s _ = Left $ "Exactly one of " ++ s
        l2j (Left x) = Just x
        l2j (Right _) = Nothing
        validateSchemes = zeroOrOne "scheme" sc
        validateLogins = exactlyOne "login" lo
        validatePasswords = exactlyOne "password" pa
        validateAccounts = zeroOrOne "account" ac

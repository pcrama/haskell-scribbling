module Parser
( Entry(..)
, ParsedEntry(..)
, netrcNamedEntry
, validateParsedEntry
)
where

import qualified Data.Text as T
import           Data.Maybe (catMaybes)
import           Data.Void (Void)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

import Password (Password, isPasswordChar, mkPassword)


data Entry = Entry {
    scheme :: Maybe T.Text
  , machine :: T.Text
  , login :: T.Text
  , account :: Maybe Password
  , password :: Password
} deriving (Show)


data ParsedEntry = ParsedEntry {
    parsedMachine :: T.Text
  , parsedSchemes :: [T.Text]
  , parsedLogins :: [T.Text]
  , parsedPasswords :: [Password]
  , parsedAccounts :: [Password]
} deriving (Show)


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


netrcNamedEntry :: Parser ParsedEntry
netrcNamedEntry = do
  machn <- netrcSpace *> netrcMachine
  acc machn [] [] [] []
  where acc m schemes logins passwords accounts = do
          mbS <- P.optional netrcScheme
          case mbS of
            Just s -> acc m (schemes ++ [s]) logins passwords accounts
            Nothing -> do
              mbL <- P.optional netrcLogin
              case mbL of
                Just l -> acc m schemes (logins ++ [l]) passwords accounts
                Nothing -> do
                  mbP <- P.optional netrcPassword
                  case mbP of
                    Just p -> acc m schemes logins (passwords ++ [p]) accounts
                    Nothing -> do
                      mbA <- P.optional netrcAccount
                      case mbA of
                        Just a -> acc m schemes logins passwords (accounts ++ [a])
                        Nothing -> return ParsedEntry {
                            parsedMachine = m
                          , parsedSchemes = schemes
                          , parsedLogins = logins
                          , parsedPasswords = passwords
                          , parsedAccounts = accounts
                          }


validateParsedEntry :: ParsedEntry -> Either [String] Entry
validateParsedEntry (ParsedEntry {
    parsedMachine = ma
  , parsedSchemes = sc
  , parsedLogins = lo
  , parsedPasswords = pa
  , parsedAccounts = ac
  }) = case (validateSchemes, validateLogins, validatePasswords, validateAccounts) of
         (Right schm, Right lgn, Right psswrd, Right accnt) ->
           Right Entry { machine = ma
                       , scheme = schm
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

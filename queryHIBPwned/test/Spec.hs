module Main where

import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Maybe (fromJust, maybe)
import qualified Data.Text as T
import           Test.Hspec
import           Text.Megaparsec (runParser, errorBundlePretty)

import Parser
import Password
import AppMonadSpec


testValidPassword :: (String, String, L8.ByteString, SHA1Prefix) -> SpecWith ()
testValidPassword (pw, sh, sha1, prefix) = describe (pw ++ " " ++ L8.unpack sha1) $ do
  let wrongSha1 = "ffffffffffffffffffffffffffffffffffffffff"
  let mbP = mkPassword $ T.pack pw
  it "can be wrapped" $ maybe False (const True) mbP
  case mbP of
    Just p -> do
      it "matches correct sha1"
       $ compareWithSha1 p sha1
      it "does not match wrong sha1"
       $ not $ compareWithSha1 p wrongSha1
      it "returns first 5 nibbles of SHA1"
       $ sha1Prefix p `shouldBe` prefix
      it "masks password in output"
       $ (show p) `shouldBe` sh
    Nothing -> return ()


compareParsedEntries :: ParsedEntry -> ParsedEntry -> SpecWith ()
compareParsedEntries observed expected = do
  it "parses schemes" $
    parsedSchemes observed `shouldBe` parsedSchemes expected
  it "parses logins" $
    parsedLogins observed `shouldBe` parsedLogins expected
  it "parses passwords" $
    comparePasswordLists parsedPasswords observed expected
  it "parses accounts" $
    comparePasswordLists parsedAccounts observed expected
  where comparePasswordLists extract o e = go (extract o) (extract e)
        go [] [] = True
        go (op:os) (ep:es) = comparePasswords op ep && go os es
        go _ _ = False


testNetrcNamedEntry :: (Maybe String, T.Text, (T.Text, ParsedEntry)) -> SpecWith ()
testNetrcNamedEntry (mbTitle, input, (expectedMachine, expected)) =
  let eitherObserved = runParser netrcNamedEntry "test case" input in
  describe (maybe (T.unpack input) id mbTitle) $
    case eitherObserved of
      Left peb -> it "parses input successfully" $
        errorBundlePretty peb == ""
      Right (obsMachine, observed) -> do
        it "parses machine" $ obsMachine `shouldBe` expectedMachine
        compareParsedEntries observed expected


comparePasswords :: Password -> Password -> Bool
comparePasswords o e = (sha1Prefix o, show o) == (sha1Prefix e, show e)


testValidateParsedEntry :: Either [String] Entry -> ParsedEntry -> SpecWith ()
testValidateParsedEntry expected entry = do
  case (observed, expected) of
    (Left obsErr, Left expErr) -> it "has same error messages" $ obsErr `shouldBe` expErr
    (Right obsEntry, Right expEntry) -> do
      it "has same scheme" $ scheme obsEntry `shouldBe` scheme expEntry
      it "has same login" $ login obsEntry `shouldBe` login expEntry
      it "probably has same password" $ comparePasswords (password obsEntry) (password expEntry)
      case (account obsEntry, account expEntry) of
        (Just o, Just e) -> it "probably has same account" $ comparePasswords o e
        (Nothing, Nothing) -> it "has no account data" $ True
        _ -> it "has same account data" $ False
    (_, _) -> it "should reject/accept same values" $ False
  where observed = validateParsedEntry entry


testParseNetrc :: (Maybe String, T.Text, Netrc ParsedEntry) -> SpecWith ()
testParseNetrc (mbTitle, input, expected) =
  let eitherObserved = runParser netrcParser "test case" input in
  describe (maybe (T.unpack input) id mbTitle) $
    case eitherObserved of
      Left peb -> it "parses input successfully" $
        errorBundlePretty peb `shouldBe` ""
      Right observed ->
        let toLabeledList = foldrNetrc (:) []
            expectedList = toLabeledList expected
            observedList = toLabeledList observed
            expectedLen = length expectedList
            observedLen = length observedList in
        if expectedLen /= observedLen
        then it "parses the same amount of entries" $
               observedLen `shouldBe` expectedLen
        else flip mapM_ (zip observedList expectedList) $ \((mbObsM, obs), (mbXpcM, xpc)) ->
               let xpcM = maybe "<default>" T.unpack mbXpcM in
               describe xpcM $ do
                 it (case mbXpcM of
                       Just _ -> "parses the same machine name"
                       Nothing -> "recognizes the 'default' entry") $
                   mbObsM `shouldBe` mbXpcM
                 compareParsedEntries obs xpc


main :: IO ()
main = hspec $ do
  describe "Password" $ do
    mapM_ testValidPassword
          [("Test1234",
            "********",
            -- echo -n "Test1234" | sha1sum
            "dddd5d7b474d2c78ebbb833789c4bfd721edf4bf",
            -- only 5 nibbles ------v this one is always 0
            SHA1Prefix 0xdd 0xdd 0x50),
           -- Example from https://www.troyhunt.com/ive-just-launched-pwned-passwords-version-2/
           ("P@ssw0rd",
            "********",
            "21BD12DC183F740EE76F27B78EB39C8AD972A757",
            SHA1Prefix 0x21 0xbd 0x10),
           ("ThisIsMyPassword",
            "****************",
            -- echo -n "ThisIsMyPassword" | sha1sum
            "30b8bd5829888900d15d2bbe6270d9bc65b0702f",
            -- only 5 nibbles ------v this one is always 0
            SHA1Prefix 0x30 0xb8 0xb0)]
    flip mapM_ [(" ", "Space"),
                ("\x80", "Codepoint >= 128")] $ \(pw, title) ->
           it ("only accepts ASCII chars (reject " ++ title ++ ")") $
             maybe True (const False) $ mkPassword $ T.pack pw
  describe "Parser" $ do
    describe "netrcNamedEntry" $ do
      mapM_ testNetrcNamedEntry
            [ (Nothing
              , "machine M scheme S login L password P"
              , ("M"
                , ParsedEntry { parsedSchemes = ["S"]
                             , parsedLogins = ["L"]
                             , parsedPasswords = [fromJust $ mkPassword "P"]
                             , parsedAccounts = []}))
            , (Just "Example with newlines"
              , "machine M\n\tscheme S\n\tlogin L\n\taccount A\n\tpassword P\n\taccount AA\n"
              , ("M"
                , ParsedEntry { parsedSchemes = ["S"]
                              , parsedLogins = ["L"]
                              , parsedPasswords = [fromJust $ mkPassword "P"]
                              , parsedAccounts = [fromJust $ mkPassword "A"
                                                 , fromJust $ mkPassword "AA"]}))
            , (Just "Example with comments"
              , "machine M # \xe7omm\xe9nts\n#login L # are\nlogin # allowed\nL password P password PP ####"
              , ("M"
                , ParsedEntry { parsedSchemes = []
                              , parsedLogins = ["L"]
                              , parsedPasswords = [fromJust $ mkPassword "P"
                                                  , fromJust $ mkPassword "PP"]
                              , parsedAccounts = []}))]
    describe "validateParsedEntry" $ do
      let (Just pw) = mkPassword "Password"
      let (Just ac) = mkPassword "Account"
      describe "Example 1" $
        testValidateParsedEntry (Right $ Entry { scheme = Just "https"
                                               , login = "L"
                                               , password = pw
                                               , account = Just ac
                                               })
                                ParsedEntry { parsedSchemes = ["https"]
                                            , parsedLogins = ["L"]
                                            , parsedPasswords = [pw]
                                            , parsedAccounts = [ac]
                                            }
      describe "Example 2" $
        testValidateParsedEntry (Right $ Entry { scheme = Nothing
                                               , login = "L"
                                               , password = ac
                                               , account = Nothing
                                               })
                                ParsedEntry { parsedSchemes = []
                                            , parsedLogins = ["L"]
                                            , parsedPasswords = [ac]
                                            , parsedAccounts = []
                                            }
      describe "Example 3" $
        testValidateParsedEntry (Left ["0 or 1 of scheme"
                                      , "login is mandatory"
                                      , "Exactly one of password"
                                      , "0 or 1 of account"])
                                ParsedEntry { parsedSchemes = ["https", "ftp"]
                                            , parsedLogins = []
                                            , parsedPasswords = [pw, ac]
                                            , parsedAccounts = [ac, ac, ac]
                                            }
      describe "Example 4" $
        testValidateParsedEntry (Left ["Exactly one of login"
                                      , "password is mandatory"])
                                ParsedEntry { parsedSchemes = ["validation"]
                                            , parsedLogins = ["a", "error"]
                                            , parsedPasswords = []
                                            , parsedAccounts = []
                                            }
    describe "foldrNetrc" $ do
      it "transforms Netrc into a list" $
        (foldrNetrc (:) [] $ Netrc [("M1", ()), ("M2", ())] $ Just ((), [("M3", ())]))
        `shouldBe`
        [(Just "M1", ()), (Just "M2", ()), (Nothing, ()), (Just "M3", ())]
    describe "parseNetrc" $ do
      -- it "works for an example" $
      --  (show $ runParser netrcParser "test case" 
      mapM_ testParseNetrc
            [ (Nothing
              , "machine M1 login L1 password P1 machine M2 login L2 password P2"
              , Netrc [ ("M1"
                        , ParsedEntry { parsedSchemes = []
                                      , parsedLogins = ["L1"]
                                      , parsedPasswords = [fromJust $ mkPassword "P1"]
                                      , parsedAccounts = []
                                      })
                      , ("M2"
                        , ParsedEntry { parsedSchemes = []
                                      , parsedLogins = ["L2"]
                                      , parsedPasswords = [fromJust $ mkPassword "P2"]
                                      , parsedAccounts = []
                                      })
                     ]
                     Nothing)
            , (Nothing
              , "machine M1 login L1 password P1 default login L2 password P2"
              , Netrc [("M1"
                       , ParsedEntry { parsedSchemes = []
                                     , parsedLogins = ["L1"]
                                     , parsedPasswords = [fromJust $ mkPassword "P1"]
                                     , parsedAccounts = []})]
                    $ Just (ParsedEntry { parsedSchemes = []
                                        , parsedLogins = ["L2"]
                                        , parsedPasswords = [fromJust $ mkPassword "P2"]
                                        , parsedAccounts = []
                                        }
                           , []))
            , (Just "Example with leading & trailing comment"
              , "# leading comment\nmachine M1\n  login L1\n   password P1\n# this is the default\ndefault # and probably hides\n  login L2 #other entries\n  password P2\nmachine M3\nmachine M4 # trailing comment\nmachine M5 account A5 # trailing comment"
              , Netrc [("M1"
                       , ParsedEntry { parsedSchemes = []
                                     , parsedLogins = ["L1"]
                                     , parsedPasswords = [fromJust $ mkPassword "P1"]
                                     , parsedAccounts = []})]
                    $ Just (ParsedEntry { parsedSchemes = []
                                        , parsedLogins = ["L2"]
                                        , parsedPasswords = [fromJust $ mkPassword "P2"]
                                        , parsedAccounts = []
                                        }
                           , [ ("M3"
                               , ParsedEntry { parsedSchemes = []
                                             , parsedLogins = []
                                             , parsedPasswords = []
                                             , parsedAccounts = []})
                             , ("M4"
                               , ParsedEntry { parsedSchemes = []
                                             , parsedLogins = []
                                             , parsedPasswords = []
                                             , parsedAccounts = []})
                             , ("M5"
                               , ParsedEntry { parsedSchemes = []
                                             , parsedLogins = []
                                             , parsedPasswords = []
                                             , parsedAccounts = [fromJust $ mkPassword "A5"]})]))
            ]
  testAppMonad

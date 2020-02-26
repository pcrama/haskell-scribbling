module Main where

import           Data.Maybe (fromJust, maybe)
import qualified Data.Text as T
import           Test.Hspec
import           Text.Megaparsec (runParser, errorBundlePretty)

import Parser
import Password

testValidPassword :: (String, String, String, SHA1Prefix) -> SpecWith ()
testValidPassword (pw, sh, sha1, prefix) = describe (pw ++ " " ++ sha1) $ do
  let wrongSha1 = "ffffffffffffffffffffffffffffffffffffffff"
  let mbP = mkPassword $ T.pack pw
  it "can be wrapped" $ maybe False (const True) mbP
  case mbP of
    Just p -> do
      it "matches correct sha1"
       $ compareWithSha1 p sha1
      it "does not match wrong sha1"
       $ not $ compareWithSha1 p wrongSha1
      it "returns first 3 bytes of SHA1"
       $ sha1Prefix p `shouldBe` prefix
      it "masks password in output"
       $ (show p) `shouldBe` sh
    Nothing -> return ()


testNetrcNamedEntry :: (Maybe String, T.Text, ParsedEntry) -> SpecWith ()
testNetrcNamedEntry (mbTitle, input, expected) =
  let eitherObserved = runParser netrcNamedEntry "test case" input in
  describe (maybe (T.unpack input) id mbTitle) $
    case eitherObserved of
      Left peb -> it "parses input successfully" $
        errorBundlePretty peb == ""
      Right observed -> do
        it "parses machine" $
          parsedMachine observed `shouldBe` parsedMachine expected
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


comparePasswords :: Password -> Password -> Bool
comparePasswords o e = (sha1Prefix o, show o) == (sha1Prefix e, show e)


testValidateParsedEntry :: Either [String] Entry -> ParsedEntry -> SpecWith ()
testValidateParsedEntry expected entry = do
  case (observed, expected) of
    (Left obsErr, Left expErr) -> it "has same error messages" $ obsErr `shouldBe` expErr
    (Right obsEntry, Right expEntry) -> do
      it "has same machine" $ machine obsEntry `shouldBe` machine expEntry
      it "has same scheme" $ scheme obsEntry `shouldBe` scheme expEntry
      it "has same login" $ login obsEntry `shouldBe` login expEntry
      it "probably has same password" $ comparePasswords (password obsEntry) (password expEntry)
      case (account obsEntry, account expEntry) of
        (Just o, Just e) -> it "probably has same account" $ comparePasswords o e
        (Nothing, Nothing) -> it "has no account data" $ True
        _ -> it "has same account data" $ False
    (_, _) -> it "should reject/accept same values" $ False
  where observed = validateParsedEntry entry


main :: IO ()
main = hspec $ do
  describe "Password" $ do
    mapM_ testValidPassword
          [("Test1234",
            "********",
            -- echo -n "Test1234" | sha1sum
            "dddd5d7b474d2c78ebbb833789c4bfd721edf4bf",
            SHA1Prefix 0xdd 0xdd 0x5d),
           ("ThisIsMyPassword",
            "****************",
            -- echo -n "ThisIsMyPassword" | sha1sum
            "30b8bd5829888900d15d2bbe6270d9bc65b0702f",
            SHA1Prefix 0x30 0xb8 0xbd)]
    flip mapM_ [(" ", "Space"),
                ("\x80", "Codepoint >= 128")] $ \(pw, title) ->
           it ("only accepts ASCII chars (reject " ++ title ++ ")") $
             maybe True (const False) $ mkPassword $ T.pack pw
  describe "Parser" $ do
    describe "netrcNamedEntry" $ do
      mapM_ testNetrcNamedEntry
            [ (Nothing
              , "machine M scheme S login L password P"
              , ParsedEntry { parsedMachine = "M"
                            , parsedSchemes = ["S"]
                            , parsedLogins = ["L"]
                            , parsedPasswords = [fromJust $ mkPassword "P"]
                            , parsedAccounts = []})
            , (Just "Example with newlines"
              , "machine M\n\tscheme S\n\tlogin L\n\taccount A\n\tpassword P\n\taccount AA\n"
              , ParsedEntry { parsedMachine = "M"
                            , parsedSchemes = ["S"]
                            , parsedLogins = ["L"]
                            , parsedPasswords = [fromJust $ mkPassword "P"]
                            , parsedAccounts = [fromJust $ mkPassword "A"
                                               , fromJust $ mkPassword "AA"]})
            , (Just "Example with comments"
              , "##\nmachine M # \xe7omm\xe9nts\n#login L # are\nlogin # allowed\nL password P password PP ####"
              , ParsedEntry { parsedMachine = "M"
                            , parsedSchemes = []
                            , parsedLogins = ["L"]
                            , parsedPasswords = [fromJust $ mkPassword "P"
                                                , fromJust $ mkPassword "PP"]
                            , parsedAccounts = []})]
    describe "validateParsedEntry" $ do
      let (Just pw) = mkPassword "Password"
      let (Just ac) = mkPassword "Account"
      describe "Example 1" $
        testValidateParsedEntry (Right $ Entry { machine = "M"
                                               , scheme = Just "https"
                                               , login = "L"
                                               , password = pw
                                               , account = Just ac
                                               })
                                ParsedEntry { parsedMachine = "M"
                                            , parsedSchemes = ["https"]
                                            , parsedLogins = ["L"]
                                            , parsedPasswords = [pw]
                                            , parsedAccounts = [ac]
                                            }
      describe "Example 2" $
        testValidateParsedEntry (Right $ Entry { machine = "M"
                                               , scheme = Nothing
                                               , login = "L"
                                               , password = ac
                                               , account = Nothing
                                               })
                                ParsedEntry { parsedMachine = "M"
                                            , parsedSchemes = []
                                            , parsedLogins = ["L"]
                                            , parsedPasswords = [ac]
                                            , parsedAccounts = []
                                            }
      describe "Example 3" $
        testValidateParsedEntry (Left ["0 or 1 of scheme"
                                      , "login is mandatory"
                                      , "Exactly one of password"
                                      , "0 or 1 of account"])
                                ParsedEntry { parsedMachine = "M"
                                            , parsedSchemes = ["https", "ftp"]
                                            , parsedLogins = []
                                            , parsedPasswords = [pw, ac]
                                            , parsedAccounts = [ac, ac, ac]
                                            }

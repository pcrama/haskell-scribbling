module Main where

import Data.Text as T
import Test.Hspec

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

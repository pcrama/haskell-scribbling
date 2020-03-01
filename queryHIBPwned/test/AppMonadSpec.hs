module AppMonadSpec (
  testAppMonad
)
where

import           Control.Monad.RWS (MonadWriter, RWS, ask, evalRWS, tell)
import           Data.Char (intToDigit)
import           Data.Maybe (fromJust, maybe)
import qualified Data.Text as T
import           Test.Hspec

import AppMonad
import Password


data AppMonadTestEnv = AMTE T.Text [(SHA1Prefix, Either String [(String, Int)])] [(T.Text, Either String [String])]


data TestAction = QPassword SHA1Prefix | QUser T.Text | Log String
  deriving (Show, Eq)

tell1 :: MonadWriter [a] m => a -> m ()
tell1 = tell . (:[])

type RW r w = RWS r w ()


instance AppMonad (RW AppMonadTestEnv [TestAction]) where
  readNetrc f = do
    AMTE input _ _ <- ask
    return $ f input
  queryPassword sha1p = do
    tell1 $ QPassword sha1p
    AMTE _ ps _ <- ask
    case lookup sha1p ps of
      Just r -> return r
      Nothing -> return $ Left $ (show sha1p) <> " not found."
  queryUsername u = do
    tell1 $ QUser u
    AMTE _ _ us <- ask
    case lookup u us of
      Just r -> return r
      Nothing -> return $ Left $ (T.unpack u) <> " not found."
  putLog = tell1 . Log


mkTestEnv :: T.Text -> [(SHA1Prefix, Either String [(String, Int)])] -> [(T.Text, Either String [String])] -> AppMonadTestEnv
mkTestEnv input passwords users =
  AMTE input
       (map prependSHA1prefix passwords)
       users
  where prependSHA1prefix (p, errorMsg@(Left _)) = (p, errorMsg)
        prependSHA1prefix (p@(SHA1Prefix a b c), Right result) =
          let toS x tl = (intToDigit $ fromIntegral x `div` 16):(intToDigit $ fromIntegral x `mod` 16):tl
              prefix = toS a . toS b . toS c
              modifyFirst f (x, y) = (f x, y) in
          (p, Right $ map (modifyFirst prefix) result)


-- for x in Test1234 password PassWord P@ssw0rd LetMeIn W34kP@55w0rd ApiFailure ; do echo -n "$x" | sha1sum | sed -e 's/\(..\)\(..\)\(..\)\([^ ]*\).*/-- (SHA1Prefix 0x\1 0x\2 0x\3, Right [("\4", 1)])/'; done
-- (SHA1Prefix 0xdd 0xdd 0x5d, Right [("7b474d2c78ebbb833789c4bfd721edf4bf", 1)])
-- (SHA1Prefix 0x5b 0xaa 0x61, Right [("e4c9b93f3f0682250b6cf8331b7ee68fd8", 1)])
-- (SHA1Prefix 0xf5 0xd8 0xcf, Right [("270e82cff012babb306319ffa13983a45d", 1)])
-- (SHA1Prefix 0x21 0xbd 0x12, Right [("dc183f740ee76f27b78eb39c8ad972a757", 1)])
-- (SHA1Prefix 0x1a 0x3c 0xfa, Right [("b60ad0b26227f0fbc349049c1a045ba486", 1)])
-- (SHA1Prefix 0xfd 0xf4 0xef, Right [("89f02ae8c051f74eb2ee2547616ac0139b", 1)])
-- (SHA1Prefix 0xb0 0x96 0x51, Right [("9f26d388d060453cf6f9bcbfe2f3244e1f", 1)])
passwordDatabase :: [(SHA1Prefix, Either String [(String, Int)])]
passwordDatabase = [
    (SHA1Prefix 0xdd 0xdd 0x5d, Right [("7b474d2c78ebbb833789c4bfd721edf4bf", 1)
                                      ,("ffffffffffffffffffffffffffffffffff", 2)
                                      ,("0000000000000000000000000000000000", 3)])
  , (SHA1Prefix 0x5b 0xaa 0x61, Right [("e4c9b93f3f0682250b6cf8331b7ee68fd8", 1)
                                      ,("ffffffffffffffffffffffffffffffffff", 2)
                                      ,("0000000000000000000000000000000000", 3)])
  , (SHA1Prefix 0xf5 0xd8 0xcf, Right [("270e82cff012babb306319ffa13983a45d", 1)
                                      ,("ffffffffffffffffffffffffffffffffff", 2)
                                      ,("0000000000000000000000000000000000", 3)])
  , (SHA1Prefix 0x21 0xbd 0x12, Right [("dc183f740ee76f27b78eb39c8ad972a757", 1)
                                      ,("ffffffffffffffffffffffffffffffffff", 2)
                                      ,("0000000000000000000000000000000000", 3)])
  , (SHA1Prefix 0x1a 0x3c 0xfa, Right [("b60ad0b26227f0fbc349049c1a045ba486", 1)
                                      ,("ffffffffffffffffffffffffffffffffff", 2)
                                      ,("0000000000000000000000000000000000", 3)])
  , (SHA1Prefix 0xfd 0xf4 0xef, Right [("89f02ae8c051f74eb2ee2547616ac0139b", 1)
                                      ,("ffffffffffffffffffffffffffffffffff", 2)
                                      ,("0000000000000000000000000000000000", 3)])
  , (SHA1Prefix 0xb0 0x96 0x51, Left "400 HTTP error code")
  -- echo -n "ImagineThisPasswordHasNeverBeenHacked" | sha1sum
  , (SHA1Prefix 0xc9 0x4f 0x63, Right [-- ("d599fdf54038eea16cd266f8337c510b88", 0),
                                       ("ffffffffffffffffffffffffffffffffff", 2)
                                      ,("0000000000000000000000000000000000", 3)])
  ]


userDatabase :: [(T.Text, Either String [String])]
userDatabase = [
    ("leaked-com@unsafe.example.com", Right ["Security breach at example.com"])
  , ("leaked-org@unsafe.example.org", Right ["Security breach at example.org"
                                            ,"Other leak"])
  , ("safe@fort.knox", Right [])
  , ("api-error@404.com", Left "404 API error")
  ]


runTest :: Maybe String
        -> T.Text
        -> [(SHA1Prefix, Either String [(String, Int)])]
        -> [(T.Text, Either String [String])]
        -> [TestAction]
        -> SpecWith ()
runTest title netrcText passwords users expectedActions =
  let amte = mkTestEnv netrcText passwords users
      (_, observedActions) = evalRWS queryHIBPwned amte () in
  describe (maybe (T.unpack netrcText) id title) $ do
    it "makes same password queries" $
      filter isPasswordQuery observedActions `shouldBe` filter isPasswordQuery expectedActions
    it "makes same user queries" $
      filter isUserQuery observedActions `shouldBe` filter isUserQuery expectedActions
  where isPasswordQuery (QPassword _) = True
        isPasswordQuery _ = False
        isUserQuery (QUser _) = True
        isUserQuery _ = False


testAppMonad :: SpecWith()
testAppMonad = describe "AppMonad" $ do
  runTest Nothing
          "machine M1 login safe@fort.knox password password"
          passwordDatabase
          userDatabase
          [QPassword $ sha1Prefix $ fromJust $ mkPassword "password"
          , QUser "safe@fort.knox"
          ]
  runTest (Just "reuse user name, query only once")
          "machine M1 login safe@fort.knox password password machine M2 login safe@fort.knox password W34kP@55w0rd account Test1234"
          passwordDatabase
          userDatabase
          [QPassword $ sha1Prefix $ fromJust $ mkPassword "password"
          , QPassword $ sha1Prefix $ fromJust $ mkPassword "W34kP@55w0rd"
          , QPassword $ sha1Prefix $ fromJust $ mkPassword "Test1234"
          , QUser "safe@fort.knox"
          ]
  runTest (Just "reuse password, queried each time")
          "machine M1 login safe@fort.knox password password account password machine M2 login safe@fort.knox password password account password"
          passwordDatabase
          userDatabase
          [QPassword $ sha1Prefix $ fromJust $ mkPassword "password"
          , QPassword $ sha1Prefix $ fromJust $ mkPassword "password"
          , QPassword $ sha1Prefix $ fromJust $ mkPassword "password"
          , QPassword $ sha1Prefix $ fromJust $ mkPassword "password"
          , QUser "safe@fort.knox"
          ]
  runTest Nothing
          "machine M1 login leaked-org@unsafe.example.org password ApiFailure machine M2 login leaked-com@unsafe.example.com password LetMeIn"
          passwordDatabase
          userDatabase
          [QPassword $ sha1Prefix $ fromJust $ mkPassword "ApiFailure"
          , QPassword $ sha1Prefix $ fromJust $ mkPassword "LetMeIn"
          , QUser "leaked-org@unsafe.example.org"
          , QUser "leaked-com@unsafe.example.com"
          ]
  runTest Nothing
          "machine M1 password ImagineThisPasswordHasNeverBeenHacked login api-error@404.com"
          passwordDatabase
          userDatabase
          [QPassword $ sha1Prefix $ fromJust $ mkPassword "ImagineThisPasswordHasNeverBeenHacked"
          , QUser "api-error@404.com"
          ]
  runTest Nothing
          "machine provoke a parse error"
          passwordDatabase
          userDatabase
          []
  runTest Nothing
          "machine provoke login a scheme validation login error"
          passwordDatabase
          userDatabase
          []

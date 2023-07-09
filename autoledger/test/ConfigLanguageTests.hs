{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
module ConfigLanguageTests (
  configLanguageSpecs
  ) where

import           Test.Hspec
import qualified Data.Text as T
import Data.Time.Calendar (fromGregorian)
import Control.Monad (void)
import Control.Monad.Reader (runReader)
import Control.Monad.Except (runExceptT, runExcept)
import Data.Either (isLeft)
import Text.Parsec (sourceName, sourceLine, sourceColumn)
import Text.Parsec.Error (errorPos)

import Lib
import TransactionTest
import ConfigLanguage (
  CompilerError, CompilerErrorMessage(..), TEType(..), Value(..), ParsePosition, ParsedValue, runValueParser, compile)

testParser :: (Show a, Eq a) => String -> (ParsedValue -> Value a) -> Maybe (Value a) -> SpecWith ()
testParser s f e = case e of
  Nothing -> it ("Parse '" <> s <> "' expecting failure") $ parse `shouldSatisfy` isLeft
  Just expected -> it ("Parse '" <> s <> "'") $ fmap f parse `shouldBe` Right expected
  where parse = runValueParser s (T.pack s)

dropExtra :: ParsedValue -> Value ()
dropExtra = void

testParseThenCompileFile :: String -- ^ "source name" identifying the test
  -> String -- ^ input "program" to compile, assumed to parse correctly
  -> Either (CompilerError ParsePosition) Classifiers
testParseThenCompileFile src s = do
  prog <- mapError $ parseConfigFileText src $ T.pack s
  runExcept (compileConfigFile (src, 0, 0) prog)
  where mapError (Left x) = let pos = errorPos x in
                              Left (Msg $ "Unexpected parse error: " <> show x
                                   , (sourceName pos, sourceLine pos, sourceColumn pos))
        mapError (Right r) = Right r

testParseThenCompile :: String -- ^ "source name" identifying the test
  -> String -- ^ input "program" to compile, assumed to parse correctly
  -> Either (CompilerError ParsePosition) (Compiled ParsePosition)
testParseThenCompile src s = do
  prog <- mapError $ runValueParser src $ T.pack s
  runReader (runExceptT (compile prog)) env
  where mapError (Left x) = let pos = errorPos x in
                              Left (Msg $ "Unexpected parse error: " <> show x
                                   , (sourceName pos, sourceLine pos, sourceColumn pos))
        mapError (Right r) = Right r
        p = ("unit test pre defined", 1, 1)
        env "test-string" = Just $ AsText p $ Constant $ T.pack "test-value"
        env "test-true" = Just $ AsBool p $ Constant True
        env "test-false" = Just $ AsBool p $ Constant False
        env "test-pair" =
          Just $ AsTextPair p $ Pair (Constant $ T.pack "1st") $ Constant $ T.pack "2nd"
        env _ = Nothing

configLanguageSpecs :: SpecWith ()
configLanguageSpecs = describe "src/ConfigLanguage" $ do
  let vCons x y = Cons x y ()
  let vSym = (`Sym` ())
  let vStr = (`Str` ())
  let vNil = Nil ()
  let vList1 = (`vCons` vNil)
  let vList = foldr vCons vNil
  describe "valueParser" $ do
    testParser "()" id $ Just $ Nil ("()", 1, 1)
    testParser "nil" id $ Just $ Nil ("nil", 1, 1)
    testParser "abc" id $ Just $ Sym "abc" ("abc", 1, 1)
    testParser "\"abc\"" id $ Just $ Str "abc" ("\"abc\"", 1, 1)
    testParser "\"a\\\"bc\"" id $ Just $ Str "a\"bc" ("\"a\\\"bc\"", 1, 1)
    testParser "\"a\\&b\"" dropExtra $ Just $ Str "ab" ()
    testParser "\"a\\\n \\  b\"" dropExtra $ Just $ Str "a  b" ()
    testParser "\"a\\n\\ \\  b\"" dropExtra $ Just $ Str "a\n  b" ()
    let s = "(a\n \"b\")" in
      testParser s id $ Just $ Cons (Sym "a" (s, 1, 2))
                                     (Cons (Str "b" (s, 2, 2)) (Nil ("dummy", 0, 0)) (s, 2, 2))
                                     (s, 1, 1)
    testParser "(" id Nothing
    testParser "\"a" id Nothing
    testParser "\"a\n\"" id Nothing
    testParser "*" id Nothing
    testParser "((a \"b c\") (an-identifier_with-1-2-3 () nil) ((nil)))"
               dropExtra
             $ Just
             $ vCons (vCons (vSym "a") $ vCons (vStr "b c") vNil)
                   $ vCons (vCons (vSym "an-identifier_with-1-2-3")
                                $ vCons vNil $ vList1 vNil)
                         $ vList1 $ vList1 $ vList1 vNil
    testParser "( (a \"b c\"\n) ( an-identifier_with-1-2-3 ( ) nil\n)\t ((\tnil\n)))"
               dropExtra
             $ Just
             $ vCons (vCons (vSym "a") $ vCons (vStr "b c") vNil)
                   $ vCons (vCons (vSym "an-identifier_with-1-2-3")
                                $ vCons vNil $ vList1 vNil)
                         $ vList1 $ vList1 $ vList1 vNil
  describe "parseConfigFileText" $ do
    it "Test 1"
    $ (map dropExtra
       <$> parseConfigFileText "test 1" "(define a ())\n(define b \"c\")")
      `shouldBe` Right [vList [vSym "define", vSym "a", vNil]
                       , vList [vSym "define", vSym "b", vStr "c"]]
  let ev cp a o d = evalForTransaction cp $ Transaction {
                                              _account = a,
                                              _otherAccount = mkNonBlankText o,
                                              _description = mkNonBlankText d,
                                              _otherName = Nothing,
                                              _amountCents = 0,
                                              _date = fromGregorian 2022 3 30,
                                              _identifyingComment = "This is a test",
                                              _currency = "EUR" }
  let itIsOk = it "compiles correctly" $ True `shouldBe` True
  let shouldBeDiff c = it "Is the expected compilation result" $
        ("Unexpected compilation result: " <> show c) `shouldBe` "different"
  let doTest s f = describe s $ f $ testParseThenCompile s s
  describe "compile (cond form)" $ do
    doTest "(cond test-string (t test-string))" $ \case
      Right (AsText _ (Cond (Constant "test-value") [(Constant True,Constant "test-value")])) -> itIsOk
      c -> shouldBeDiff c
    doTest "(cond test-false (nil test-true))" $ \case
      Right (AsBool _ (Cond (Constant False) [(Constant False,Constant True)])) -> itIsOk
      c -> shouldBeDiff c
    doTest "(cond test-pair (t \"a\"))" $ \case
      Left (TypeError _ TEPair TEString, _) -> itIsOk
      c -> shouldBeDiff c
    doTest "(cond nil (t (pair \"a\" \"b\")))" $ \case
      Left (TypeError _ TEBool TEPair, _) -> itIsOk
      c -> shouldBeDiff c
    doTest "(cond \"a\" (t nil))" $ \case
      Left (TypeError _ TEString TEBool, _) -> itIsOk
      c -> shouldBeDiff c
    doTest "(cond test-pair ((contains description account) (pair \"a\" \"b\")) ((contains account description) (pair \"c\" \"d\")))" $ \case
      Right (AsTextPair _
             (Cond (Pair _ _)
              [(ContainsCaseInsensitive Description Account,Constant ("a","b"))
              ,(ContainsCaseInsensitive Account Description,Constant ("c","d"))])) ->
        itIsOk
      c -> shouldBeDiff c
  describe "compile (lookup form)" $ do
    doTest "(lookup test-pair () (pair account (pair nil description)))" $ \case
      Right (AsTextPair _ (Pair (Constant "1st") (Constant "2nd"))) -> itIsOk
      c -> shouldBeDiff c
    doTest "(lookup test-pair ((\"a\" (pair \"s\" \"t\"))) account)" $ \case
      Right (AsTextPair _ cp) -> context "compiled code works" $ do
        it "ex1" $ ev cp "a" "?" "?" `shouldBe` ("s", "t")
        it "ex2" $ ev cp "b" "?" "?" `shouldBe` ("1st", "2nd")
      c -> shouldBeDiff c
  describe "compile (bool)" $ do
    doTest "(and (contains account \"a\") (contains description \"d\"))" $ \case
      Right (AsBool _ cp) -> context "compiled code works" $ do
        it "a, d -> True" $ ev cp "a" "o" "d" `shouldBe` True
        it "a, x -> False" $ ev cp "a" "o" "x" `shouldBe` False
        it "x, d -> False" $ ev cp "x" "o" "d" `shouldBe` False
        it "x, y -> False" $ ev cp "x" "o" "y" `shouldBe` False
      c -> it "Is the expected compilation result" $
        ("Unexpected compilation result: " <> show c) `shouldBe` "And (Contains Account (Constant \"a\")) (Contains Description (Constant \"b\"))"
    doTest "(or (cond t ((and (contains account \"a\") (contains description \"d\")) nil)) () nil (contains other-account \"oa\"))" $ \case
      Right (AsBool _ cp) -> context "compiled code works" $ do
        it "a, oa, d -> True" $ ev cp "a" "oa" "d" `shouldBe` True
        it "a, x, d -> False" $ ev cp "a" "x" "d" `shouldBe` False
        it "a, x, y -> True" $ ev cp "a" "x" "y" `shouldBe` True
        it "x, y, z -> True" $ ev cp "x" "y" "z" `shouldBe` True
      c -> shouldBeDiff c
  describe "compile (pair)" $ do
    doTest "(pair (snd (pair \"a\" \"z\")) (fst (pair \"y\" \"c\")))" $ \case
      Right (AsTextPair _ cp) -> it "compiled code works" $ ev cp "a" "b" "c" `shouldBe` ("z", "y")
      c ->
        it "Is the expected compilation result" $
          ("Unexpected compilation result: " <> show c) `shouldBe` "different"
    doTest "test-pair" $ \case
      Right (AsTextPair _ (Pair (Constant "1st") (Constant "2nd")))
        -> it "is compiled correctly" $ True `shouldBe` True
      c ->
        it "Is the expected compilation result" $
          ("Unexpected compilation result: " <> show c) `shouldBe` "different"
    doTest "(pair test-string (fst (pair account description)))" $ \case
      Right (AsTextPair _ (Pair (Constant "test-value") (Fst (Pair Account Description)))) ->
          it "is compiled correctly" $ True `shouldBe` True
      c ->
        it "Is the expected compilation result" $
          ("Unexpected compilation result: " <> show c) `shouldBe` "different"
  describe "compile (text)" $ do
    doTest "(lookup account ((\"a\" \"b\")) description)" $ \case
      Right (AsText _ (Select Account f Description)) -> context "lookup function works" $ do
        it "key \"a\" is found" $ f "a" `shouldBe` Just "b"
        it "key \"b\" is not found" $ f "b" `shouldBe` Nothing
      c ->
        it "Is the expected compilation result" $
          ("Unexpected compilation result: " <> show c) `shouldBe` "Select Acccount f Description"
    doTest "(lookup (cond other-account ((contains account \"a\") \"a\") ((contains description \"b\") \"b\")) ((\"a1\" \"r1\") (\"a2\" \"r2\")) (fst (pair account other-account)))" $ \case
      Right (AsText _ cp) -> context "compiled code works" $ do
        it "a1 -> r1" $ ev cp "a1" "o" "d" `shouldBe` "r1"
        it "a2 -> r2" $ ev cp "a2" "o" "d" `shouldBe` "r2"
        it "a3 -> a" $ ev cp "a3" "o" "d" `shouldBe` "a"
        it "4b (desc=4b) -> a" $ ev cp "4b" "o" "4b" `shouldBe` "b"
        it "z (desc=z) -> o" $ ev cp "z" "o" "z" `shouldBe` "o"
      c -> shouldBeDiff c
    doTest "test-true" $ \case
      Right (AsBool _ (Constant x)) -> it "is compiled correctly" $ x `shouldBe` True
      c -> shouldBeDiff c
    doTest "test-false" $ \case
      Right (AsBool _ (Constant x)) -> it "is compiled correctly" $ x `shouldBe` False
      c -> shouldBeDiff c
    doTest "test-string" $ \case
      Right (AsText _ (Constant x)) -> it "is compiled correctly" $ x `shouldBe` "test-value"
      c -> shouldBeDiff c
    doTest "\
         \(cond (cond (fst test-pair)\n\
         \            (test-false \"never seen\")\n\
         \            ((contains description test-string) description))\n\
         \      ((and test-true\n\
         \            (contains account \"a\")\n\
         \            (or test-false (contains \"a\" account)))\n\
         \       \"account is 'a'\")\n\
         \      ((or (contains description \"b\") (contains other-account \"b\"))\n\
         \       (lookup (snd test-pair)\n\
         \               ((\"zzz\" \"= zzz\"))\n\
         \               account)))" $ \case
      Right (AsText _ cp) -> context "compiled code works" $ do
        it "ex1" $ ev cp "x" "x" "see test-value in descr" `shouldBe` "see test-value in descr"
        it "ex2" $ ev cp "x" "x" "x" `shouldBe` "1st"
        it "ex3" $ ev cp "x" "b" "x" `shouldBe` "2nd"
        it "ex4" $ ev cp "zzz" "b" "x" `shouldBe` "= zzz"
        it "ex5" $ ev cp "zzz" "x" "b" `shouldBe` "= zzz"
        it "ex6" $ ev cp "a" "?" "?" `shouldBe` "account is 'a'"
      c -> shouldBeDiff c
  describe "testParseThenCompileFile" $ do
    let doTestFile n s f = describe n $ f $ testParseThenCompileFile n s
    doTestFile "no ledger-asset" "(setq ledger-other-asset \"o\")\n\
                                 \(setq b \"b\")\n\
                                 \(setq ledger-text \"t\")\n" $ \case
      Left (Msg "Missing assignment to ledger-asset", _) -> itIsOk
      Left e -> it "failed with the correct error message" $ show e `shouldBe` "different"
      Right c -> shouldBeDiff c
    doTestFile "no ledger-text nor ledger-other-asset"
               "(setq ledger-asset \"a\")" $ \case
      Left (Msg "Missing assignment to ledger-text ledger-other-asset", _) -> itIsOk
      Left e -> it "failed with the correct error message" $ show e `shouldBe` "different"
      Right c -> shouldBeDiff c
    doTestFile "valid ex1"
      "(setq info (cond (pair description \"Expenses:\")\n\
      \                 ((contains other-account \"restaurant\") (pair description \"Expenses:Food\"))\n\
      \                 ((contains other-account \"doctor\") (pair \"Doctor visit\"\n\
      \                                                            (cond \"Expenses:Health\"\n\
      \                                                                  ((contains description \"990101\") \"Expenses:Health:MrA\")\n\
      \                                                                  ((contains description \"000202\") \"Expenses:Health:MsB\"))))))\n\
      \(setq ledger-asset (lookup \"Asset:\" ((\"BE51-1234\" \"Asset:Bank\") (\"BE62-5678\" \"Asset:Savings\")) account))\n\
      \(setq ledger-text (fst info))\n\
      \(setq ledger-other-asset (snd info))" $ \case
      Left e -> it "should parse & compile correctly" $ show e `shouldBe` "different"
      Right c -> context "compiled values evaluate correctly" $ do
        context "Asset classifier" $ do
          let go s e = it s $ ev (getAssetClassifier c) (T.pack s) "?" "?" `shouldBe` e
          go "BE51-1234" "Asset:Bank"
          go "BE62-5678" "Asset:Savings"
          go "BE73-0001" "Asset:"
        context "Ledger text classifier" $ do
          let go d o e = it (d <> " -> " <> o) $ ev (getLedgerTextClassifier c) "?" (T.pack o) (T.pack d) `shouldBe` e
          go "descr" "?" "descr"
          go "?" "doctor" "Doctor visit"
          go "pizza vesuvio" "restaurant" "pizza vesuvio"
        context "Other asset classifier" $ do
          let go d o e = it (d <> " -> " <> o) $ ev (getLedgerOtherAssetClassifier c) "?" (T.pack o) (T.pack d) `shouldBe` e
          go "descr" "?" "Expenses:"
          go "?" "doctor" "Expenses:Health"
          go "Surgery for patient 990101" "doctor" "Expenses:Health:MrA"
          go "Cast for patient 000202" "doctor" "Expenses:Health:MsB"
          go "Blood test for patient 680923" "doctor" "Expenses:Health"
          go "pizza vesuvio" "restaurant" "Expenses:Food"

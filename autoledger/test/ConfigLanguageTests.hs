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
import Control.Monad.Except (runExceptT)
import Data.Either (isLeft)
import Text.Parsec (sourceName, sourceLine, sourceColumn)
import Text.Parsec.Error (errorPos)

import Lib
import TransactionTest
import ConfigLanguage (
  CompilerError, CompilerErrorMessage(..), Value(..), ParsePosition, ParsedValue, runValueParser)

testParser :: (Show a, Eq a) => String -> (ParsedValue -> Value a) -> Maybe (Value a) -> SpecWith ()
testParser s f e = case e of
  Nothing -> it ("Parse '" <> s <> "' expecting failure") $ parse `shouldSatisfy` isLeft
  Just expected -> it ("Parse '" <> s <> "'") $ fmap f parse `shouldBe` Right expected
  where parse = runValueParser s (T.pack s)

dropExtra :: ParsedValue -> Value ()
dropExtra = void

testParseThenCompile :: String -- ^ "source name" identifying the test
  -> String -- ^ input "program" to compile, assumed to parse correctly
  -> Either (CompilerError ParsePosition) Compiled
testParseThenCompile src s = do
  p <- mapError $ runValueParser src $ T.pack s
  runReader (runExceptT (compile p)) env
  where mapError (Left x) = let pos = errorPos x in
                              Left (Msg $ "Unexpected parse error: " <> show x
                                   , (sourceName pos, sourceLine pos, sourceColumn pos))
        mapError (Right r) = Right r
        env "test-string" = Just $ AsText $ Constant $ T.pack "test-value"
        env "test-true" = Just $ AsBool $ Constant True
        env "test-false" = Just $ AsBool $ Constant False
        env "test-pair" = Just $ AsPair (AsText $ Constant $ T.pack "1st")
                                      $ AsPair (AsBool $ Constant False)
                                               (AsText $ Constant $ T.pack "2nd")
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
                                              _currency = "EUR" }
  describe "compile (cond form)" $ do
    let itIsOk = it "compiles correctly" $ True `shouldBe` True
    let shouldBeDiff c = it "Is the expected compilation result" $
          ("Unexpected compilation result: " <> show c) `shouldBe` "different"
    let doTest s f = describe s $ f $ testParseThenCompile s s
    doTest "(cond test-string (t test-string))" $ \case
      Right (AsText (Cond (Constant "test-value") ((Constant True),(Constant "test-value")))) -> itIsOk
      c -> shouldBeDiff c
    doTest "(cond test-false (nil test-true))" $ \case
      Right (AsText (Cond (Constant False) ((Constant false),(Constant True)))) -> itIsOk
      c -> shouldBeDiff c
  describe "compile (bool)" $ do
    case testParseThenCompile "example0" "(and (contains account \"a\") (contains description \"d\"))" of
      Right (AsBool cp) -> context "compiled code works" $ do
        it "a, d -> True" $ ev cp "a" "o" "d" `shouldBe` True
        it "a, x -> False" $ ev cp "a" "o" "x" `shouldBe` False
        it "x, d -> False" $ ev cp "x" "o" "d" `shouldBe` False
        it "x, y -> False" $ ev cp "x" "o" "y" `shouldBe` False
      c -> it "Is the expected compilation result" $
        ("Unexpected compilation result: " <> show c) `shouldBe` "And (Contains Account (Constant \"a\")) (Contains Description (Constant \"b\"))"
    case testParseThenCompile "example1" "(or (cond t ((and (contains account \"a\") (contains description \"d\")) nil)) () nil (contains other-account \"oa\"))" of
      Right (AsBool cp) -> context "compiled code works" $ do
        it "a, oa, d -> True" $ ev cp "a" "oa" "d" `shouldBe` True
        it "a, x, d -> False" $ ev cp "a" "x" "d" `shouldBe` False
        it "a, x, y -> True" $ ev cp "a" "x" "y" `shouldBe` True
        it "x, y, z -> True" $ ev cp "x" "y" "z" `shouldBe` True
      c -> it "Is the expected compilation result" $
        ("Unexpected compilation result: " <> show c) `shouldBe` "different"
  describe "compile (pair)" $ do
    case testParseThenCompile "example2" "(pair (pair \"a\" nil) (fst (pair \"b\" \"c\")))" of
      Right (AsPair (AsPair (AsText (Constant "a")) (AsBool (Constant False)))
                    (AsText (Constant "b"))) ->
        it "has the correct value" $ True `shouldBe` True -- already checked by pattern match
      c -> it "Is the expected compilation result" $
        ("Unexpected compilation result: " <> show c) `shouldBe` "different"
  describe "compile (text)" $ do
    case testParseThenCompile "example3" "(lookup account ((\"a\" \"b\")) description)" of
      Right (AsText (Select Account f Description)) -> context "lookup function works" $ do
        it "key \"a\" is found" $ f "a" `shouldBe` Just "b"
        it "key \"b\" is not found" $ f "b" `shouldBe` Nothing
      c -> it "Is the expected compilation result" $
        ("Unexpected compilation result: " <> show c) `shouldBe` "Select Acccount f Description"
    case testParseThenCompile "example4" "(lookup (cond other-account ((contains account \"a\") \"a\") ((contains description \"b\") \"b\")) ((\"a1\" \"r1\") (\"a2\" \"r2\")) (fst (pair account other-account)))" of
      Right (AsText cp) -> context "compiled code works" $ do
        it "a1 -> r1" $ ev cp "a1" "o" "d" `shouldBe` "r1"
        it "a2 -> r2" $ ev cp "a2" "o" "d" `shouldBe` "r2"
        it "a3 -> a" $ ev cp "a3" "o" "d" `shouldBe` "a"
        it "4b (desc=4b) -> a" $ ev cp "4b" "o" "4b" `shouldBe` "b"
        it "z (desc=z) -> o" $ ev cp "z" "o" "z" `shouldBe` "o"
      c -> it "Is the expected compilation result" $
        ("Unexpected compilation result: " <> show c) `shouldBe` "different"
    case testParseThenCompile "example5" "test-true" of
      Right (AsBool (Constant x)) -> it "is compiled correctly" $ x `shouldBe` True
      c -> it "Is the expected compilation result" $
        ("Unexpected compilation result: " <> show c) `shouldBe` "different"
    case testParseThenCompile "example6" "test-false" of
      Right (AsBool (Constant x)) -> it "is compiled correctly" $ x `shouldBe` False
      c -> it "Is the expected compilation result" $
        ("Unexpected compilation result: " <> show c) `shouldBe` "different"
    case testParseThenCompile "example7" "test-string" of
      Right (AsText (Constant x)) -> it "is compiled correctly" $ x `shouldBe` "test-value"
      c -> it "Is the expected compilation result" $
        ("Unexpected compilation result: " <> show c) `shouldBe` "different"
    case testParseThenCompile "example7" "test-pair" of
      Right (AsPair (AsText (Constant "1st")) (AsPair (AsBool (Constant False)) (AsText (Constant "2nd")))) -> it "is compiled correctly" $ True `shouldBe` True
      c -> it "Is the expected compilation result" $
        ("Unexpected compilation result: " <> show c) `shouldBe` "different"
    case testParseThenCompile "example8" "(pair test-string (pair test-false test-true))" of
      Right (AsPair (AsText (Constant "test-value"))
                    (AsPair (AsBool (Constant False))
                            (AsBool (Constant True)))) ->
        it "is compiled correctly" $ True `shouldBe` True
      c -> it "Is the expected compilation result" $
        ("Unexpected compilation result: " <> show c) `shouldBe` "different"
    case testParseThenCompile "example9" "\
         \(cond (cond test-pair\n\
         \            (test-false \"never seen\")\n\
         \            ((contains description test-string) description)\n\
         \ )\n\
         \      ((and test-true\n\
         \            (contains account \"a\")\
         \            (or test-false (contains \"a\" account)))\
         \       \"account is 'a'\")\
         \      ((or (contains description \"b\") (contains other-account \"b\"))\
         \       (lookup (snd (snd test-pair))\
         \               ((\"zzz\" \"= zzz\"))\
         \               account)))" of
      Right (AsText cp) -> context "compiled code works" $ do
        it "ex1" $ ev cp "x" "x" "see test-value in descr" `shouldBe` "see test-value in descr"
        it "ex2" $ ev cp "x" "x" "x" `shouldBe` "test-value"
        it "ex3" $ ev cp "x" "b" "x" `shouldBe` "2nd"
        it "ex4" $ ev cp "zzz" "b" "x" `shouldBe` "= zzz"
        it "ex5" $ ev cp "zzz" "x" "b" `shouldBe` "= zzz"
        it "ex6" $ ev cp "a" "?" "?" `shouldBe` "account is 'a'"
        it "ex7" $ ev cp "aha" "?" "?" `shouldBe` "test-value"
        it "ex8" $ ev cp "pick fst" "?" "?" `shouldBe` "1st"
        it "ex9" $ ev cp "snd" "bbb" "ccc" `shouldBe` "2nd"
      c -> it "Is the expected compilation result" $
        ("Unexpected compilation result: " <> show c) `shouldBe` "different"

-- Local Variables:
-- compile-command: "([ -r autoledger.cabal ] || cd ..; cabal new-test)"
-- coding: utf-8
-- End:

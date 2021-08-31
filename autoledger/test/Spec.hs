module Main where

import Data.Functor.Identity (Identity)
-- import qualified Data.Text as T
import           Data.Text (Text)
import           Test.Hspec
import           Text.Parsec (
  ParseError
  , eof
  , runParser)

import Lib


parseWithEofAtEnd :: (Show a, Eq a)
                  => UnstructuredParser Identity a
                  -> String
                  -> UnstructuredParsingState
                  -> Text
                  -> Either ParseError a
                  -> SpecWith ()
parseWithEofAtEnd p nm s input expected =
  it nm $ runParser (p <* eof) s nm input `shouldBe` expected

testParseUnstructuredDataSingleRow :: SpecWith ()
testParseUnstructuredDataSingleRow = describe "parseUnstructuredDataSingleRow" $ do
  flip mapM_ [("parses an empty row as a single empty column", "", Right [""])
             ,("parses a row without empty columns", "a;b;c", Right ["a", "b", "c"])
             ,("parses a row with an empty column in the middle", "a;;c", Right ["a", "", "c"])
             ,("parses a row with an empty column at the end", "a;b;c;d;e;", Right ["a", "b", "c", "d", "e", ""])]
           $ \(name, input, expected) ->
                 parseWithEofAtEnd parseUnstructuredDataSingleRow
                                   name
                                   Nothing
                                   input
                                   expected

testParseUnstructuredHeaderLine :: SpecWith ()
testParseUnstructuredHeaderLine = describe "parseUnstructuredHeaderLine" $ do
  flip mapM_ [("parses an unstructured header"
              , "header;value"
              , Right $ UnstructuredHeader {uhLine = 1, uhKey = "header", uhValue = "value"})
             ,("strips whitespace"
              , "  header  ; value  "
              , Right $ UnstructuredHeader {uhLine = 1, uhKey = "header", uhValue = "value"})
             ,("handles empty value"
              , "  header  ;"
              , Right $ UnstructuredHeader {uhLine = 1, uhKey = "header", uhValue = ""})
             ,("handles empty value (after stripping spaces)"
              , "  header  ;   "
              , Right $ UnstructuredHeader {uhLine = 1, uhKey = "header", uhValue = ""})]
           $ \(name, input, expected) ->
                 parseWithEofAtEnd parseUnstructuredHeaderLine
                                   name
                                   Nothing
                                   input
                                   expected

testParseUnstructuredHeaders :: SpecWith ()
testParseUnstructuredHeaders = describe "parseUnstructuredHeaders" $ do
  flip mapM_ [("parses a set of unstructured headers without empty values"
              , "header;value\nother;something else\n;\n"
              , Right [UnstructuredHeader {uhLine = 1, uhKey = "header", uhValue = "value"}
                      , UnstructuredHeader {uhLine = 2, uhKey = "other", uhValue = "something else"}])
             ,("parses a set of unstructured headers without empty values (carriage return + newline)"
              , "header;value\r\nother;line ending\r\n;\r\n"
              , Right [UnstructuredHeader {uhLine = 1, uhKey = "header", uhValue = "value"}
                      , UnstructuredHeader {uhLine = 2, uhKey = "other", uhValue = "line ending"}])
             ,("parses a set of unstructured headers with empty first value"
              , "header;\nother;value\n;\n"
              , Right [UnstructuredHeader {uhLine = 1, uhKey = "header", uhValue = ""}
                      , UnstructuredHeader {uhLine = 2, uhKey = "other", uhValue = "value"}])
             ,("parses a set of unstructured headers with only empty values"
              , "header;  \nother;\n;\n"
              , Right [UnstructuredHeader {uhLine = 1, uhKey = "header", uhValue = ""}
                      , UnstructuredHeader {uhLine = 2, uhKey = "other", uhValue = ""}])]
           $ \(name, input, expected) ->
                 parseWithEofAtEnd parseUnstructuredHeaders
                                   name
                                   Nothing
                                   input
                                   expected

testParseUnstructuredDataRows :: SpecWith ()
testParseUnstructuredDataRows = describe "parseUnstructuredDataRows" $ do
  flip mapM_ [("parses 1 data row"
              , Just 3
              , "a;b;c"
              , Right [["a", "b", "c"]])
             ,("parses 2 data rows"
              , Just 3
              , "a;b;c\nde;fg;hi"
              , Right [["a", "b", "c"], ["de", "fg", "hi"]])
             ,("parses 2 data rows with trailing newline"
              , Just 3
              , "a;b;c\nde;fg;hi\n"
              , Right [["a", "b", "c"], ["de", "fg", "hi"]])
             ,("parses data row ignoring trailing blank column"
              , Just 3
              , "a;b;c;"
              , Right [["a", "b", "c"]])]
           $ \(name, state, input, expected) ->
                 parseWithEofAtEnd parseUnstructuredDataRows
                                   name
                                   state
                                   input
                                   expected

testParseUnstructuredData :: SpecWith ()
testParseUnstructuredData = describe "parseUnstructuredData" $ do
    parseWithEofAtEnd parseUnstructuredData
                      "parses some data (no trailiing newline)"
                      Nothing
                      "header;value\nother;\n;\ncol1;col2;col3\nv1;v2;v3"
                    $ Right ([UnstructuredHeader {uhLine = 1, uhKey = "header", uhValue = "value"}
                              ,UnstructuredHeader {uhLine = 2, uhKey = "other", uhValue = ""}]
                            ,["col1","col2","col3"]
                            ,[["v1","v2","v3"]])
    parseWithEofAtEnd parseUnstructuredData
                      "parses some data (with trailiing newline)"
                      Nothing
                      "header;value\nother;\n;\ncol1;col2;col3\nv1;v2;v3\n"
                    $ Right ([UnstructuredHeader {uhLine = 1, uhKey = "header", uhValue = "value"}
                              ,UnstructuredHeader {uhLine = 2, uhKey = "other", uhValue = ""}]
                            ,["col1","col2","col3"]
                            ,[["v1","v2","v3"]])

main :: IO ()
main = hspec $ do
  describe "Unstructured parsing (examples from playing in REPL)" $ do
    testParseUnstructuredDataSingleRow
    testParseUnstructuredDataRows
    testParseUnstructuredHeaderLine
    testParseUnstructuredHeaders
    testParseUnstructuredData
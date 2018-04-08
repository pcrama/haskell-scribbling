import Control.Applicative (Alternative(..), some, many)

import Test.Hspec
import Test.HUnit

import qualified SimpleParser as S
import qualified MTParser1 as M1
import qualified MTParser2 as M2

data Nesting = One Char | Many [Nesting] deriving (Show, Eq)

-- code duplication... a case for backpack?
s_char :: S.Parser Char Nesting
s_char = fmap One $ S.check (not . (`elem` "()")) S.item

s_level :: S.Parser Char Nesting
s_level = S.literal '(' *> fmap Many (many s_element) <* S.literal ')'

s_element :: S.Parser Char Nesting
s_element = s_char <|> s_level

s_parseNest = many s_element

s_p :: S.Parser a (a, a)
s_p = (,) <$> S.item <*> S.item

s_q :: Eq a => a -> S.Parser a (a, a)
s_q x = S.item >>= (\y -> if x == y then return (x, x) else fmap ((,) y) S.item)

testSimpleParser s p ts expected =
  let parsed = S.getParser p ts
  in assertEqual (s ++ " " ++ show ts ++ " = " ++ (show $ parsed))
                 expected
                 parsed

-- code duplication... a case for backpack?
mt_char :: M1.Parser Char Nesting
mt_char = fmap One $ M1.check (not . (`elem` "()")) M1.item

mt_level :: M1.Parser Char Nesting
mt_level = M1.literal '(' *> fmap Many (many mt_element) <* M1.literal ')'

mt_element :: M1.Parser Char Nesting
mt_element = mt_char <|> mt_level

mt_parseNest = many mt_element

mt_p :: M1.Parser a (a, a)
mt_p = (,) <$> M1.item <*> M1.item

mt_q :: Eq a => a -> M1.Parser a (a, a)
mt_q x = M1.item >>= (\y -> if x == y then return (x, x) else fmap ((,) y) M1.item)

testM1Parser s p ts expected =
  let parsed = M1.getParser p ts
  in assertEqual (s ++ " " ++ show ts ++ " = " ++ (show $ parsed))
                 expected
                 parsed

testM2Parser :: (Eq a, Show a)
  => String -- comment
  -> M2.Parser String Char a -- parser
  -> String -- input
  -> Either String (Maybe (a, String)) -- expected value
  -> Assertion
testM2Parser s p ts expected =
  let parsed = M2.getParser p ts
  in assertEqual (s ++ " " ++ show ts ++ " = " ++ (show $ parsed))
                 expected
                 parsed

multiTest assertion descr parser inputAndExpected =
  mapM_ (uncurry $ assertion descr parser) inputAndExpected

m2TestDataValidParses =
  [ ("a b c {define q (f x)}",
      Right $ Just ([ M1.ASymbol "a"
                    , M1.ASymbol "b"
                    , M1.ASymbol "c"
                    , M1.ADefine "q" $ M1.AApp (M1.ASymbol "f") [M1.ASymbol "x"]]
                   , ""))
  , ("{ ; this is an open curly brace\n\
     \  define    \n\
     \  a b; this a symbol and another}\n\
     \}  \n\n; done\n\n"
    , Right $ Just ([M1.ADefine "a" $ M1.ASymbol "b"], ""))
  , ("; \n {lambda {x y z} a b (c b a)}end"
    , Right $ Just ([M1.ALambda ["x","y","z"]
                      [M1.ASymbol "a"
                      ,M1.ASymbol "b"
                      ,M1.AApp (M1.ASymbol "c") [M1.ASymbol "b",M1.ASymbol "a"]]
                    ,M1.ASymbol "end"]
                   ,""))]

m2TestDataErrorReporting =
  [ ("()", Left $ M2.eAppOper M2.parserErrorDict)
  , ("a (", Left $ M2.eAppOper M2.parserErrorDict)
  , ("(a", Left $ M2.eAppClose M2.parserErrorDict)
  , ("{define (a b c)}", Left $ M2.eDefSym M2.parserErrorDict)
  , ("{define a}", Left $ M2.eDefForm M2.parserErrorDict)
  , ("{lambda (a b c)}", Left $ M2.eLamParam M2.parserErrorDict)
  , ("{lambda {a b a} (c d)}", Left $ M2.eLamDupe M2.parserErrorDict)
  , ("{lambda {a ()} (c d)}", Left $ M2.eLamPClose M2.parserErrorDict)
  , ("{lambda {a b (c d)}", Left $ M2.eLamPClose M2.parserErrorDict)
  , ("{lambda {a b}}", Left $ M2.eLamBody M2.parserErrorDict)
  , ("{define x y", Left $ M2.eSpecClose M2.parserErrorDict)
  , ("{defin x y}", Left $ M2.eSpecial M2.parserErrorDict)
  , ("a,b", Left $ M2.eWoof M2.parserErrorDict)
  ]

m2ToM1Example (input, expected) = (input, either (const Nothing) id expected)

main :: IO ()
main = hspec $ do
  describe "SimpleParser" $ do
    it "should work for some examples" $ do
      testSimpleParser "s_p" s_p [1] $ Nothing
      testSimpleParser "s_q 1" (s_q 1) [1] $ Just ([], (1,1))
      multiTest testSimpleParser
                "s_q 1 <|> s_q 2"
                (s_q 1 <|> s_q 2)
                [ ([1], Just ([],(1,1)))
                , ([2], Just ([],(2,2)))
                , ([3,4], Just ([],(3,4)))
                , ([5], Nothing) ]
      multiTest testSimpleParser
                "fmap (+) (s_check (==1)) <*> s_check (==3)"
                (fmap (+) (S.check (==1) S.item) <*> S.check (==3) S.item)
                [ ([1,2], Nothing)
                , ([1,3,5], Just ([5],4))
                , ([1], Nothing)
                , ([2], Nothing)
                , ([3], Nothing)
                , ([], Nothing)
                , ([4,5,6], Nothing) ]
      multiTest testSimpleParser
                "s_parseNest"
                s_parseNest
                [ ("abc", Just ("",[One 'a',One 'b',One 'c']))
                , ("(ab)", Just ("",[Many [One 'a',One 'b']]))
                , ("((((a)b))", Just ("((((a)b))",[]))
                , ("((a))b", Just ("",[Many [Many [One 'a']],One 'b'])) ]
  describe "MTParser" $ do
    it "should work for some examples" $ do
      testM1Parser "mt_p" mt_p [1] $ Nothing
      testM1Parser "mt_q 1" (mt_q 1) [1] $ Just ((1,1), [])
      multiTest testM1Parser
                "mt_q 1 <|> mt_q 2"
                (mt_q 1 <|> mt_q 2)
                [ ([1], Just ((1,1),[]))
                , ([2], Just ((2,2),[]))
                , ([3,4], Just ((3,4),[]))
                , ([5], Nothing) ]
      multiTest testM1Parser
                "fmap (+) (M1.check (==1)) <*> M1.check (==3)"
                (fmap (+) (M1.check (==1) M1.item) <*> M1.check (==3) M1.item)
                [ ([1,2], Nothing)
                , ([1,3,5], Just (4,[5]))
                , ([1], Nothing)
                , ([2], Nothing)
                , ([3], Nothing)
                , ([], Nothing)
                , ([4,5,6], Nothing) ]
      multiTest testM1Parser
                "mt_parseNest"
                mt_parseNest
                [ ("abc", Just ([One 'a',One 'b',One 'c'],""))
                , ("(ab)", Just ([Many [One 'a',One 'b']],""))
                , ("((((a)b))", Just ([],"((((a)b))"))
                , ("((a))b", Just ([Many [Many [One 'a']],One 'b'],"")) ]
    it "should fail for failing MTParser2 examples" $ do
      multiTest testM1Parser
                "M1.woof"
                M1.woof
              $ map m2ToM1Example m2TestDataErrorReporting
    it "should work for some woof examples" $ do
      multiTest testM1Parser
                "M1.woof"
                M1.woof
              $ [ -- trailing comma:
                  ("a b c {define q (f x)},", Nothing)
                  -- too many forms in {define}
                , ("{define a b c}", Nothing)
                -- duplicate parameter in lambda list:
                , ("; \n {lambda {x y z y a} a b (c b a)}end", Nothing)
                , ("; \n {lambda {x y z} a b (c b a)}end"
                  , Just ([M1.ALambda ["x","y","z"]
                                      [M1.ASymbol "a"
                                      ,M1.ASymbol "b"
                                      ,M1.AApp (M1.ASymbol "c") [M1.ASymbol "b",M1.ASymbol "a"]]
                          ,M1.ASymbol "end"]
                         ,""))
                ] ++ map m2ToM1Example m2TestDataValidParses
  describe "MTParser2" $ do
    it "should work for minimal example" $ do
      multiTest testM2Parser
                "item"
                -- force type of parser to prove that the error type would be showable, too
                (M2.item :: M2.Parser String Char Char)
                  [ ("abc", (Right $ Just ('a', "bc")))
                  , ("", Right Nothing)]
    it "parses woof syntax" $ do
      multiTest testM2Parser "opencurly" M2.opencurly [ ("{a", Right $ Just ('{', "a"))
                                                      , ("b", Right Nothing)]
      multiTest testM2Parser "closecurly" M2.closecurly [ ("}", Right $ Just ('}', ""))
                                                        , ("", Right Nothing)]
    it "should report proper errors" $ do
      multiTest testM2Parser "M2.woof" M2.woof m2TestDataErrorReporting
    it "should work for some woof examples" $ do
      multiTest testM2Parser "M2.woof" M2.woof m2TestDataValidParses

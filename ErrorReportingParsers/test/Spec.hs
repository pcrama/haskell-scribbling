import Control.Applicative (Alternative(..), some, many)

import Test.Hspec
import Test.HUnit

import qualified SimpleParser as S
import qualified MTParser1 as M1

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

testMTParser s p ts expected =
  let parsed = M1.getParser p ts
  in assertEqual (s ++ " " ++ show ts ++ " = " ++ (show $ parsed))
                 expected
                 parsed

multiTest assertion descr parser inputAndExpected =
  mapM_ (uncurry $ assertion descr parser) inputAndExpected

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
      testMTParser "mt_p" mt_p [1] $ Nothing
      testMTParser "mt_q 1" (mt_q 1) [1] $ Just ((1,1), [])
      multiTest testMTParser
                "mt_q 1 <|> mt_q 2"
                (mt_q 1 <|> mt_q 2)
                [ ([1], Just ((1,1),[]))
                , ([2], Just ((2,2),[]))
                , ([3,4], Just ((3,4),[]))
                , ([5], Nothing) ]
      multiTest testMTParser
                "fmap (+) (M1.check (==1)) <*> M1.check (==3)"
                (fmap (+) (M1.check (==1) M1.item) <*> M1.check (==3) M1.item)
                [ ([1,2], Nothing)
                , ([1,3,5], Just (4,[5]))
                , ([1], Nothing)
                , ([2], Nothing)
                , ([3], Nothing)
                , ([], Nothing)
                , ([4,5,6], Nothing) ]
      multiTest testMTParser
                "mt_parseNest"
                mt_parseNest
                [ ("abc", Just ([One 'a',One 'b',One 'c'],""))
                , ("(ab)", Just ([Many [One 'a',One 'b']],""))
                , ("((((a)b))", Just ([],"((((a)b))"))
                , ("((a))b", Just ([Many [Many [One 'a']],One 'b'],"")) ]
    it "should work for some woof examples" $ do
      multiTest testMTParser
                "M1.woof"
                M1.woof
                [ -- trailing comma:
                  ("a b c {define q (f x)},", Nothing)
                , ("a b c {define q (f x)}",
                   Just ([ M1.ASymbol "a"
                         , M1.ASymbol "b"
                         , M1.ASymbol "c"
                         , M1.ADefine "q" $ M1.AApp (M1.ASymbol "f") [M1.ASymbol "x"]]
                        , ""))
                  -- too many forms in {define}
                , ("{define a b c}", Nothing)
                , ("{ ; this is an open curly brace\n\
                   \  define    \n\
                   \  a b; this a symbol and another}\n\
                   \}  \n\n; done\n\n"
                   , Just ([M1.ADefine "a" $ M1.ASymbol "b"], ""))
                -- duplicate parameter in lambda list:
                , ("; \n {lambda {x y z y a} a b (c b a)}end", Nothing)
                , ("; \n {lambda {x y z} a b (c b a)}end"
                  , Just ([M1.ALambda ["x","y","z"]
                                      [M1.ASymbol "a"
                                      ,M1.ASymbol "b"
                                      ,M1.AApp (M1.ASymbol "c") [M1.ASymbol "b",M1.ASymbol "a"]]
                          ,M1.ASymbol "end"]
                         ,""))]
      

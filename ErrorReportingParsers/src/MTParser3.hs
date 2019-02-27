{-# LANGUAGE FlexibleContexts #-}
-- Based on: Error Reporting Parsers: a Monad Transformer Approach
-- by Matt Fenwick & Jay Vyas following along
-- https://themonadreader.files.wordpress.com/2013/08/issue221.pdf
-- pp 59--61
module MTParser3
  ( Parser
  , AST(..) -- re-exported
  , getParser
  , item
  , literal
  , satisfy
  , check -- re-exported
  , opencurly
  , openparen
  , closecurly
  , closeparen
  , symbol
  , woof
  , ErrorDict(..) -- re-exported
  , Pos(..)
  , parserErrorDict -- re-exported
  )

where

import Control.Applicative (Alternative(..), some, many)
import Control.Monad.State (StateT(..), MonadState(..), lift)
import Control.Monad.Trans.Maybe (MaybeT(..))

import MTParser1
  ( AST(..)
  , Switch(..)
  , check
  )
  -- hiding (Parser, getParser)
import MTParser2
  ( commit
  , parserErrorDict
  , ErrorDict(..)
  )

-- Either e (Maybe (State [t] a))
-- -> Left error [partial parse resulting in error message]
-- -> Right Nothing [parse failure]
-- -> Right (Just (a, [t])) [parse success]
type ErrorMsg = String
data Pos = Pos { row :: Int, col :: Int }
  deriving (Show, Eq)
type Parser e t a = StateT [t] (StateT Pos (MaybeT (Either (e, Pos)))) a

instance Monoid Pos where
  mempty = Pos { row = 1, col = 1 }
  mappend a _ = a

getParser :: Parser e t a -> [t] -> Either (e, Pos) (Maybe ((a, [t]), Pos))
getParser p xs = runMaybeT $ runStateT (runStateT p xs) $ mempty

getPos :: Parser e t Pos
getPos = lift get

putPos :: Pos -> Parser e t ()
putPos = lift . put

updatePos :: (Pos -> Pos) -> Parser e t ()
updatePos f = getPos >>= (putPos . f)

item :: Parser e Char Char
item = do
  xs <- get
  case xs of
    [] -> empty
    (t:ts) -> do
      put ts
      updatePos (case t of
                   '\n' -> \Pos { row = r } -> Pos { row = r + 1, col = 1 }
                   _ -> \Pos { row = r, col = c } -> Pos { row = r, col = c + 1 })
      return t

cut :: e -> Parser e Char a -> Parser e Char a
cut message parser = do
  pos <- getPos
  commit (message, pos) parser
  
-- copy-paste from MTParser1 to pick up the modified `item' that updates the position
junk :: Monoid e => Parser e Char [[Char]]
junk = many $ whitespace <|> comment

tok :: Monoid e => Parser e Char a -> Parser e Char a
tok p = p <* junk

not1 :: (Monoid e) => Parser e Char a -> Parser e Char Char
not1 p = switch p *> item

endCheck :: Monoid e => Parser e Char ()
endCheck = switch item

satisfy :: (Char -> Bool) -> Parser e Char Char
satisfy p = do
  i <- item
  if p i then return i else empty

literal :: Char -> Parser e Char Char
literal c = satisfy (== c)

-- Token parsers (pp 48--50)
opencurly, closecurly, openparen, closeparen :: Monoid e => Parser e Char Char
opencurly = tok $ literal '{'
closecurly = tok $ literal '}'
openparen = tok $ literal '('
closeparen = tok $ literal ')'

whitespace :: Parser e Char [Char]
whitespace = some $ satisfy (`elem` " \n\t\r\f")

comment :: Monoid e => Parser e Char [Char]
comment = pure (:) <*> literal ';' <*> many (not1 $ literal '\n')

symbol :: Monoid e => Parser e Char [Char]
symbol = tok $ some char
  where char = satisfy (`elem` (['a'..'z'] ++ ['A'..'Z']))

-- Syntactic structures (was pp 50--ff for example 1, reimplemented here to report errors)
-- (Monoid e) constraint needed because of the Switch class constraint on the Parser.
form, application, special, define, lambda :: (Monoid e) => ErrorDict e -> Parser e Char AST
application ed =
  openparen *>
  (cut (eAppOper ed) $
     pure AApp      <*>
     form ed        <*>
     many (form ed) <*
     cut (eAppClose ed) closeparen)

special ed =
  opencurly *>
  (cut (eSpecial ed) $ define ed <|> lambda ed) <*
  (cut (eSpecClose ed) closecurly)

define ed =
  check (== "define") symbol *>
  (cut (eDefSym ed)
    (pure ADefine              <*>
     symbol                    <*>
     (cut (eDefForm ed) $ form ed)))

lambda ed =
    check (== "lambda") symbol       *>
    (cut (eLamParam ed) $
     pure ALambda                   <*>
     (opencurly                      *>
      distinctSymbols ed            <*
      (cut (eLamPClose ed) closecurly)) <*>
     (cut (eLamBody ed)
           $ some (form ed)))
  where distinct :: (Eq a, Foldable f) => f a -> Bool
        distinct = fst . foldr compareAndStore (True, [])
          where compareAndStore _ (False, _) = (False, [])
                compareAndStore x (True, []) = (True, [x])
                compareAndStore x (True, xs) = (not $ x `elem` xs, x:xs)
        distinctSymbols :: Monoid e => ErrorDict e -> Parser e Char [String]
        distinctSymbols e = do
          syms <- many symbol
          cut (eLamDupe e) $ check distinct (return syms)


form ed = application ed <|> special ed <|> (fmap ASymbol symbol)

woof :: Parser ErrorMsg Char [AST]
woof =
  junk                         *>
  many (form parserErrorDict) <*
  (cut (eWoof parserErrorDict) endCheck)

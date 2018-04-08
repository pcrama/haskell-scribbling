{-# LANGUAGE FlexibleContexts #-}
-- Based on: Error Reporting Parsers: a Monad Transformer Approach
-- by Matt Fenwick & Jay Vyas following along
-- https://themonadreader.files.wordpress.com/2013/08/issue221.pdf
-- pp 47--ff
module MTParser1
  ( Parser
  , AST(..)
  , getParser
  , item
  , satisfy
  , check
  , literal
  , woof
  )

where

import Control.Applicative (Alternative(..), some, many)
import Control.Monad.State (StateT(..))
import Control.Monad.State (MonadState(..))

import qualified SimpleParser as S

type Parser t a = StateT [t] Maybe a

item :: (MonadState [t] m, Alternative m) => m t
item = get >>= \xs -> case xs of
                        (t:ts) -> put ts *> return t
                        [] -> empty

satisfy :: (MonadState [t] m, Alternative m) => (t -> Bool) -> m t
satisfy = flip check item

check :: (MonadState [t] m, Alternative m) => (a -> Bool) -> m a -> m a
check = S.check

literal :: (Eq t, MonadState [t] m, Alternative m) => t -> m t
literal = satisfy . (==)

{-
Page 47:

Woof         :=   Form(+)
Form         :=   Symbol  |  Special  |  Application
Symbol       :=   [a-zA-Z](+)
Special      :=   ’{’  ( Define  |  Lambda )  ’}’
Define       :=   ’define’  Symbol  Form
Lambda       :=   ’lambda’  ’{’  Symbol(*)  ’}’  Form(+)
Application  :=   ’(’  Form(+)  ’)’
Whitespace   :=   \s+
Comment      :=   ’;’  (not ’\n’)(*)

Token : {, }, (, ), Symbol

-}

data AST -- pg 48
    = ASymbol String
    | ALambda [String] [AST]
    | ADefine String AST
    | AApp    AST [AST]
  deriving (Show, Eq)

getParser :: Parser t a -> [t] -> Maybe (a, [t])
getParser = runStateT

-- Auxiliary functions
junk :: Parser Char [[Char]]
junk = many $ whitespace <|> comment

tok :: Parser Char a -> Parser Char a
tok p = p <* junk

class Switch f where
  switch :: f a -> f ()

instance Switch Maybe where
  switch (Just _) = Nothing
  switch Nothing = Just ()

instance Switch [] where
  switch [] = [()]
  switch (_:_) = []

instance (Functor m, Switch m) => Switch (StateT s m) where
  switch (StateT f) = StateT g
    where g s = fmap (const ((), s)) . switch $ f s

not1 :: (MonadState [t] m, Alternative m, Switch m) => m a -> m t
not1 p = switch p *> item

endCheck :: Parser t ()
endCheck = switch item

-- Token parsers (pp 48--50)
opencurly, closecurly, openparen, closeparen :: Parser Char Char
opencurly = tok $ literal '{'
closecurly = tok $ literal '}'
openparen = tok $ literal '('
closeparen = tok $ literal ')'

whitespace, comment :: Parser Char [Char]
whitespace = some $ satisfy (`elem` " \n\t\r\f")

comment = pure (:) <*> literal ';' <*> many (not1 $ literal '\n')

symbol :: Parser Char [Char]
symbol = tok $ some char
  where char = satisfy (`elem` (['a'..'z'] ++ ['A'..'Z']))

--Syntactic structures (pp 50--ff)
form, application, special, define, lambda :: Parser Char AST
application =
  openparen  *>
  pure AApp <*>
  form      <*>
  many form <*
  closeparen

special =
  opencurly *> (define <|> lambda) <* closecurly

define =
  check (== "define") symbol *>
  pure ADefine              <*>
  symbol                    <*>
  form

lambda =
    check (== "lambda") symbol    *>
    opencurly                     *>
    pure ALambda                 <*>
    check distinct (many symbol) <*>
    (closecurly                   *>
    some form)
  where distinct :: (Eq a, Foldable f) => f a -> Bool
        distinct = fst . foldr compareAndStore (True, [])
          where compareAndStore x (False, _) = (False, [])
                compareAndStore x (True, []) = (True, [x])
                compareAndStore x (True, xs) = (not $ x `elem` xs, x:xs)

form = application <|> special <|> (fmap ASymbol symbol)

woof :: Parser Char [AST]
woof = junk *> many form <* endCheck

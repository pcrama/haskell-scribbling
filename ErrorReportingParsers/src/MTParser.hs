{-# LANGUAGE FlexibleContexts #-}
-- Based on: Error Reporting Parsers: a Monad Transformer Approach
-- by Matt Fenwick & Jay Vyas following along
-- https://themonadreader.files.wordpress.com/2013/08/issue221.pdf
-- pp 47--ff
module MTParser
  ( Parser
  , AST(..)
  , getParser
  , item
  , satisfy
  , check
  , literal
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

check :: (MonadState [t] m, Alternative m) => (t -> Bool) -> m t -> m t
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

opencurly, closecurly :: Parser Char Char
opencurly = literal '{'
closecurly = literal '}'

whitespace, comment :: Parser Char [Char]
whitespace = some $ satisfy (`elem` " \n\t\r\f")

comment = pure (:) <*> literal ';' <*> many (not1 $ literal '\n')

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

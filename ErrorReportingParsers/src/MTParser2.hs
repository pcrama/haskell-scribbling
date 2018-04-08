{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, FlexibleInstances, TypeFamilies #-}
-- Based on: Error Reporting Parsers: a Monad Transformer Approach
-- by Matt Fenwick & Jay Vyas following along
-- https://themonadreader.files.wordpress.com/2013/08/issue221.pdf
-- pp 53--59
module MTParser2
  ( Parser
  , AST(..)
  , getParser
  , item -- re-exported
  , literal -- re-exported
  , satisfy -- re-exported
  , check -- re-exported
  , opencurly -- re-exported
  , openparen -- re-exported
  , closecurly -- re-exported
  , closeparen -- re-exported
  , symbol -- re-exported
  , woof
  , commit
  , ErrorDict(..)
  , parserErrorDict
  )

where

import Control.Applicative (Alternative(..), some, many)
import Control.Monad.State (StateT(..), MonadState(..), lift)
import Control.Monad.Trans.Maybe (MaybeT(..))

import TheirMonadError (MonadError(..))
import MTParser1
  ( AST(..)
  , Switch(..)
  , item
  , literal
  , satisfy
  , check
  , opencurly
  , openparen
  , closecurly
  , closeparen
  , not1
  , endCheck
  , junk
  , symbol
  )
  -- hiding (Parser, getParser)


-- Either e (Maybe (State [t] a))
-- -> Left error [partial parse resulting in error message]
-- -> Right Nothing [parse failure]
-- -> Right (Just (a, [t])) [parse success]
type Parser e t a = StateT [t] (MaybeT (Either e)) a

getParser :: Parser e t a -> [t] -> Either e (Maybe (a, [t]))
getParser p xs = runMaybeT (runStateT p xs)

instance (Monoid e) => Switch (Either e) where
  switch (Left _) = Right ()
  switch (Right f) = Left mempty
    
-- more specialized type: e -> Parser e t a -> Parser e t a
commit :: (MonadError m, Alternative m) => Error m -> m a -> m a
commit e p = p <|> throwError e

-- Syntactic structures (was pp 50--ff for example 1, reimplemented here to report errors)
type ErrorMsg = String

data ErrorDict e = ErrorDict
  { eAppOper      :: e -- ()
  , eAppClose     :: e -- (a b
  , eDefSym       :: e -- {define (a b c)}
  , eDefForm      :: e -- {define a}
  , eLamParam     :: e -- {lambda (a b c)}
  , eLamParamList :: e -- {lambda {a ()} b}
  , eLamDupe      :: e -- {lambda {a b a} (c d)}
  , eLamPClose    :: e -- {lambda {a b (c d)}
  , eLamBody      :: e -- {lambda {a b}}
  , eSpecClose    :: e -- {define x y
  , eSpecial      :: e -- {defin x y}
  , eWoof         :: e -- a,b
  }

-- Error messages
parserErrorDict = ErrorDict
  { eAppOper      = "application: missing operator"              -- ()
  , eAppClose     = "application: missing close parenthesis"     -- (a b
  , eDefSym       = "define: missing symbol"                     -- {define (a b c)}
  , eDefForm      = "define: missing form"                       -- {define a}
  , eLamParam     = "lambda: missing parameter list"             -- {lambda (a b c)}
  , eLamParamList = "lambda: malformed parameter list"           -- {lambda {a ()} b}
  , eLamDupe      = "lambda: duplicate parameter names"          -- {lambda {a b a} (c d)}
  , eLamPClose    = "lambda: missing parameter list close curly" -- {lambda {a b (c d)}
  , eLamBody      = "lambda: missing body form"                  -- {lambda {a b}}
  , eSpecClose    = "special form: missing close curly"          -- {define x y
  , eSpecial      = "special form: unable to parse"              -- {defin x y}
  , eWoof         = "woof: unparsed input"                       -- a,b
  }

form, application, special, define, lambda ::
  (MonadError m, Switch m, Alternative m, MonadState [Char] m)
  => ErrorDict (Error m) -> m AST -- Parser ErrorMsg Char AST
application ed =
  openparen *>
  (commit (eAppOper ed) $
     pure AApp      <*>
     form ed        <*>
     many (form ed) <*
     commit (eAppClose ed) closeparen)

special ed =
  opencurly *>
  (commit (eSpecial ed) $ define ed <|> lambda ed) <*
  (commit (eSpecClose ed) closecurly)

define ed =
  check (== "define") symbol *>
  (commit (eDefSym ed)
    (pure ADefine              <*>
     symbol                    <*>
     (commit (eDefForm ed) $ form ed)))

lambda ed =
    check (== "lambda") symbol       *>
    (commit (eLamParam ed) $
     pure ALambda                   <*>
     (opencurly                      *>
       (commit (eLamDupe ed)
             $ check distinct
                   $ many symbol)   <*
       (commit (eLamPClose ed)
               closecurly))         <*>
     (commit (eLamBody ed)
           $ some (form ed)))
  where distinct :: (Eq a, Foldable f) => f a -> Bool
        distinct = fst . foldr compareAndStore (True, [])
          where compareAndStore x (False, _) = (False, [])
                compareAndStore x (True, []) = (True, [x])
                compareAndStore x (True, xs) = (not $ x `elem` xs, x:xs)

form ed = application ed <|> special ed <|> (fmap ASymbol symbol)

woof :: Parser ErrorMsg Char [AST]
woof =
  junk                         *>
  many (form parserErrorDict) <*
  (commit (eWoof parserErrorDict) endCheck)

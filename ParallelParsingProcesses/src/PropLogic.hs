-- See http://www.cse.chalmers.se/edu/course/course/afp/Papers/parser-claessen.pdf
module PropLogic
    ( Form(..)
    , form
    -- removed in P7ErrorReporting, parse -- re-exported
    , moduleName -- re-exported
    ) where

import Data.Char (isAlpha)
import Control.Applicative (Alternative(..))

-- import one of the parsers here (but they don't provide `look' & `munch', so maybe update `')
-- import P1 (symbol, P(..), (+++), moduleName)
-- import P2 (symbol, P, parse, (+++), moduleName)
-- import P3RemovingBind (symbol, P, parse, (+++), moduleName)
-- import P4RemovingPlus (symbol, P, parse, (+++), moduleName)
-- import P5AssociativityOfBind (symbol, P, parse, (+++), moduleName)
-- import P6LookAhead (symbol, P, munch, parse, (+++), moduleName)
import P7ErrorReporting (symbol, P, munch, (+++), moduleName)

-- symbol = SymbolBind return
-- parse = parse'
-- (+++) = (++++)

data Form = Form :& Form | Not Form | Var String
  deriving (Show, Eq)

form, atom, paren, neg, var :: P Char Form
form = do
  a <- atom
  conj a +++ return a

atom = paren +++ neg +++ var

paren = this '(' *> form <* this ')'

neg = this '-' *> fmap Not atom

var = do
  v <- munch isAlpha
  case v of
    [] -> empty
    _ -> return $ Var v

conj :: Form -> P Char Form
conj a = do
  this '&'
  b <- form
  return $ a :& b

sat :: (s -> Bool) -> P s s
sat p = do
  c <- symbol
  if p c then return c else fail "there is still input but it doesn't match the predicate"

this :: Eq c => c -> P c c
this c = sat (c ==)

module Woof (
    Woof
  , parseWoof
)
where

import Control.Applicative (Alternative(..))
import Data.Char (digitToInt, isAlpha, isAlphaNum, isDigit)
import Data.List (foldl')

import P7ErrorReporting

type WError = String
type Pos = (Int, Int)

tabWidth = 8 :: Int

pos0 :: Pos
pos0 = (0, 0)

nextPos :: Pos -> Char -> Pos
nextPos (r, c) '\n' = (r + 1, 0)
nextPos rc '\r' = rc
nextPos (r, c) '\t' = (r, (1 + (c `div` tabWidth)) * tabWidth)
nextPos (r, c) _ = (r, c + 1)

type WoofParser = P WError Char

type Var = String -- type for identifiers

data Woof = Program WoofStatement [WoofStatement]
  deriving (Show, Eq)

data WoofStatement = If WoofExpr WoofStatement (Maybe WoofStatement)
                   | For Var WoofExpr Woof
                   | Assign Var WoofExpr
                   | Block WoofStatement [WoofStatement]
  deriving (Show, Eq)

data WoofExpr = Symbol Var
              | WInt Int
              | WSum WoofExpr WoofExpr
              | WMul WoofExpr WoofExpr
  deriving (Show, Eq)

blanks :: String
blanks = " \t\n\r"

junk :: WoofParser ()
junk = munch (`elem` blanks) *> pure ()

junk1 :: WoofParser ()
junk1 = munch1 (`elem` blanks) *> pure ()

sat :: (Char -> Bool) -> WoofParser Char
sat p = symbol $ \c -> if p c then return c else failp

operator :: Char -> WoofParser Char
operator c = junk *> sat (== c)

keyword :: String -> WoofParser String
keyword [] = return []
keyword (x:xs) = (:) <$> sat (\c -> toLower c == toLower x) <*> keyword xs

woofStatement :: WoofParser WoofStatement
woofStatement = junk *> (wif <|> wfor <|> wassign <|> wblock)

wblock :: WoofParser WoofStatement
wblock = keyword "begin"
      *> (commit "At least one statement expected after `Begin'"
               $ Program
             <$> woofStatement
             <*> many woofStatement)
     <*  (commit "Need `End' to balance `Begin'" $ keyword "end")

wassign :: WoofParser WoofStatement
wassign = Assign <$> woofIdentifier <* operator '=' *> woofExpr

wif :: WoofStatement
wif = keyword "if"
   *> (If
   <$> commit "(boolean) expression expected after `If' keyword" (junk *> woofExpr)
   <*  commit "`Then' keyword mandatory for `If'" (junk *> keyword "then")
    *> commit "`Then' clause mandatory for `If'" (junk *> woofStatement)
   <*> optionalElse)

optionalElse :: WoofParser (Maybe WoofStatement)
optionalElse = try $ keyword "else" *> commit "`Else' statements missing" (junk *> woofStatement)

wfor :: WoofParser WoofStatement
wfor = keyword "for"
    *> (For
    <$> commit "Identifier expected after `For' keyword" (junk *> woofIdentifier)
    <*  commit "= expected after identifier after `For' keyword" (operator '=')
     *> woofExpr
    <*> wblock)

woofExpr :: WoofParser WoofExpr
woofExpr = junk *> (wsymbol <|> wint <|> wsum <|> wmul)

woofIdentifier :: WoofParser Var
woofIdentifier = operator '$'
              *> (commit "At least 1 digit or _ expected after $"
                       $ (:) <$> sat (\c -> isAlpha c || c == '_')
                             <*> munch (\c -> isAlphaNum c || c == '_'))

wsymbol :: WoofParser WoofExpr
wsymbol = Symbol <$> woofIdentifier

wint :: WoofParser WoofExpr
wint = do
         firstChar <- symbol
         case firstChar of
           '+' -> signedInt 1
           '-' -> signedInt $ 0-1
           digit | isDigit digit -> finishInt digit
  where digit :: WoofParser Char
        digit = sat isDigit
        signedInt sign = fmap (WInt . (* sign) . parseInt) fullInt
        finishInt firstDigit = fmap (WInt . parseInt . (firstDigit:)) trailingDigits
        fullInt = (:) <$> sat isDigit <*> trailingDigits
        trailingDigits = do
                           x <- symbol
                           case x of
                             b | b `elem` blanks -> return []
                             d | isDigit d -> fmap (d:) trailingDigits
                             a | isAlpha a -> throwError "No letters allowed in numbers"
                             _ -> return []
        parseInt = foldl' (\p c -> p * 10 + digitToInt c) 0 . reverse

wop :: Char -> (WoofExpr -> WoofExpr -> WoofExpr) -> (Char -> WError) -> WoofParser WoofExpr
wop c constr err = constr
               <$> woofExpr
               <*  operator c
                *> commit (err c) woofExpr

errorMessageForOp :: Char -> WError
errorMessageForOp c = "There must be 2 expressions around " ++ (c:"")

wmul = wop '*' WMul errorMessageForOp
wsum = wop '+' WSum errorMessageForOp

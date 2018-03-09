module TinyThreePassCompiler
    ( AST(..)
    , compile
    , parseExpr
    , pass1
    , pass2
    , pass3
    , simulate
    ) where

import Data.Char (
  isDigit
  , isAlpha
  )
import Data.List (foldl')
import Text.ParserCombinators.ReadP

data ASTparsed = ImmP Int
               | ArgP String
               | AddP ASTparsed ASTparsed
               | SubP ASTparsed ASTparsed
               | MulP ASTparsed ASTparsed
               | DivP ASTparsed ASTparsed
  deriving (Eq, Show)

data AST = Imm Int
         | Arg Int
         | Add AST AST
         | Sub AST AST
         | Mul AST AST
         | Div AST AST
  deriving (Eq, Show)

reduceAST r@(Imm _) = r
reduceAST r@(Arg _) = r
reduceAST (Add x y) =
  case (reduceAST x, reduceAST y) of
    (Imm lft, Imm rgt) -> Imm $ lft + rgt
    (Imm 0, rgt) -> rgt
    (lft, Imm 0) -> lft
    (lft, rgt) -> Add lft rgt
reduceAST (Sub x y) =
  case (reduceAST x, reduceAST y) of
    (Imm lft, Imm rgt) -> Imm $ lft - rgt
    (lft, Imm 0) -> lft
    -- Add reduces slightly better, so try to get more of it instead of Sub
    (lft, Imm imm) -> reduceAST $ Add lft $ Imm $ 0 - imm
    (lft, Sub imm@(Imm _) rgt) -> reduceAST $ Add imm $ Add lft rgt
    (Sub lft imm@(Imm _), rgt) -> reduceAST $ Sub lft $ Add imm rgt
    (lft, rgt) -> Sub lft rgt
reduceAST (Mul x y) =
  case (reduceAST x, reduceAST y) of
    (Imm lft, Imm rgt) -> Imm $ lft * rgt
    (Imm 0, _) -> Imm 0
    (_, Imm 0) -> Imm 0
    (Imm 1, rgt) -> rgt
    (lft, Imm 1) -> lft
    (Imm lft, Mul (Imm rgtLft) rgt) -> reduceAST $ Mul rgt $ Imm $ lft * rgtLft
    (Imm lft, Mul rgt (Imm rgtRgt)) -> reduceAST $ Mul rgt $ Imm $ lft * rgtRgt
    (Mul (Imm lftLft) lft, Imm rgt) -> reduceAST $ Mul lft $ Imm $ lftLft * rgt
    (Mul lft (Imm lftRgt), Imm rgt) -> reduceAST $ Mul lft $ Imm $ lftRgt * rgt
    (lft, rgt) -> Mul lft rgt
reduceAST (Div x y) =
  case (reduceAST x, reduceAST y) of
    (Imm lft, Imm rgt) -> Imm $ lft `div` rgt
    (Imm 0, _) -> Imm 0
    (lft, Imm 1) -> lft
    (lft, rgt) -> Div lft rgt

imm :: ReadP ASTparsed
imm = skipSpaces
   >> (fmap (ImmP . read) (munch1 isDigit)
       +++ ((char '(' >> skipSpaces) *> expr <* (skipSpaces >> char ')'))
       +++ arg)

argName :: ReadP String
argName = munch1 isAlpha

arg :: ReadP ASTparsed
arg =
     skipSpaces
  *> fmap ArgP argName

addsub :: ReadP (ASTparsed -> ASTparsed -> ASTparsed)
addsub = do
  skipSpaces
  c <- satisfy (`elem` "+-")
  case c of
    '+' -> return AddP
    '-' -> return SubP
    _ -> pfail -- not reached

muldiv = do
  skipSpaces
  c <- satisfy (`elem` "*/")
  case c of
    '*' -> return MulP
    '/' -> return DivP
    _ -> pfail -- not reached

factor = chainl1 imm muldiv

expr = chainl1 factor addsub

data Function a = Function [String] ASTparsed
  deriving (Eq, Show)

cataAST :: Applicative f => (Int -> f AST) -> (String -> f AST) -> ASTparsed -> f AST
cataAST fImm _ (ImmP x) = fImm x
cataAST _ fArg (ArgP a) = fArg a
cataAST fImm fArg (AddP x y) = Add <$> (cataAST fImm fArg x) <*> (cataAST fImm fArg y)
cataAST fImm fArg (SubP x y) = Sub <$> (cataAST fImm fArg x) <*> (cataAST fImm fArg y)
cataAST fImm fArg (MulP x y) = Mul <$> (cataAST fImm fArg x) <*> (cataAST fImm fArg y)
cataAST fImm fArg (DivP x y) = Div <$> (cataAST fImm fArg x) <*> (cataAST fImm fArg y)

validateAST :: [String] -> ASTparsed -> Either String AST
validateAST args =
    cataAST (Right . Imm) (getIndex 0 args)
  where getIndex :: Int -> [String] -> String -> Either String AST
        getIndex _ [] needle = Left $ "'" ++ needle ++ "' unknown in " ++ show args
        getIndex n (hay:stack) needle | hay == needle = Right $ Arg n
                                      | otherwise = getIndex (n+1) stack needle

function = do
  skipSpaces
  char '['
  skipSpaces
  args <- sepBy argName (munch1 (==' '))
  skipSpaces
  char ']'
  fmap (Function args) expr

parseExpr s = case readP_to_S (function <* skipSpaces) s of
                [] -> Left $ "Unable to parse '" ++ s ++ "'"
                parses ->
                  case filter (null . snd) parses of
                    [] -> Left $ "Unable to parse '" ++ s ++ "': " ++ (show $ length parses) ++ " incomplete parses"
                    (Function args astString, _):_ -> case validateAST args astString of
                                                        Left err -> Left err
                                                        Right astInt -> Right $ astInt

pass1 :: String -> AST
pass1 s = case parseExpr s of
            Right result
              -> result
            r -> error $ show r

pass2 :: AST -> AST
pass2 = reduceAST

pass3 :: AST -> [String]
pass3 (Imm n) = ["IM " ++ show n]
pass3 (Arg n) = ["AR " ++ show n]
pass3 (Add x y) = (pass3 x) ++ ["PU"] ++ (pass3 y) ++ ["SW", "PO", "AD"]
pass3 (Sub x y) = (pass3 x) ++ ["PU"] ++ (pass3 y) ++ ["SW", "PO", "SU"]
pass3 (Mul x y) = (pass3 x) ++ ["PU"] ++ (pass3 y) ++ ["SW", "PO", "MU"]
pass3 (Div x y) = (pass3 x) ++ ["PU"] ++ (pass3 y) ++ ["SW", "PO", "DI"]

compile = pass3 . pass2 . pass1

{-
The third pass of the compiler is pass3. The pass3 method takes in an
Abstract Syntax Tree and returns an array of strings. Each string is
an assembly directive. You are working on a small processor with two
registers (R0 and R1), a stack, and an array of input arguments. The
result of a function is expected to be in R0. The processor supports
the following instructions:

    "IM n"     // load the constant value n into R0
    "AR n"     // load the n-th input argument into R0
    "SW"       // swap R0 and R1
    "PU"       // push R0 onto the stack
    "PO"       // pop the top value off of the stack into R0
    "AD"       // add R1 to R0 and put the result in R0
    "SU"       // subtract R1 from R0 and put the result in R0
    "MU"       // multiply R0 by R1 and put the result in R0
    "DI"       // divide R0 by R1 and put the result in R0

So, one possible return value from pass3 given the Abstract Syntax Tree shown above from pass2 is:
    (Add (Arg 0) (Imm 10))
    [ "IM 10", "SW", "AR 0", "AD" ]

Here is a simulator for the target machine. It takes an array of assembly instructions and an array of arguments and returns the result.
-}

simulate :: [String] -> [Int] -> Int
simulate asm argv = takeR0 $ foldl' step (0, 0, []) asm where
  step (r0,r1,stack) ins =
    case ins of
      ('I':'M':xs) -> (read xs, r1, stack)
      ('A':'R':xs) -> (argv !! n, r1, stack) where n = read xs
      "SW" -> (r1, r0, stack)
      "PU" -> (r0, r1, r0:stack)
      "PO" -> (head stack, r1, tail stack)
      "AD" -> (r0 + r1, r1, stack)
      "SU" -> (r0 - r1, r1, stack)
      "MU" -> (r0 * r1, r1, stack)
      "DI" -> (r0 `div` r1, r1, stack)
  takeR0 (r0,_,_) = r0

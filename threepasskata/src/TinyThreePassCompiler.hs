module TinyThreePassCompiler
    ( ASTa(..)
    , AST
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

data ASTa a = Imm Int
            | Arg a
            | Add (ASTa a) (ASTa a)
            | Sub (ASTa a) (ASTa a)
            | Mul (ASTa a) (ASTa a)
            | Div (ASTa a) (ASTa a)
  deriving (Eq, Show)

type AST = ASTa Int

reduceAST r@(Imm _) = r
reduceAST r@(Arg _) = r
reduceAST (Add (Imm x) (Imm y)) = Imm $ x + y
reduceAST (Sub (Imm x) (Imm y)) = Imm $ x - y
reduceAST (Mul (Imm x) (Imm y)) = Imm $ x * y
reduceAST (Div (Imm x) (Imm y)) = Imm $ x `div` y
reduceAST (Add x y) = Add (reduceAST x) (reduceAST y)
reduceAST (Sub x y) = Sub (reduceAST x) (reduceAST y)
reduceAST (Mul x y) = Mul (reduceAST x) (reduceAST y)
reduceAST (Div x y) = Div (reduceAST x) (reduceAST y)

imm :: ReadP (ASTa String)
imm = skipSpaces
   >> (fmap (Imm . read) (munch1 isDigit)
       +++ ((char '(' >> skipSpaces) *> expr <* (skipSpaces >> char ')'))
       +++ arg)

argName :: ReadP String
argName = munch1 isAlpha

arg :: ReadP (ASTa String)
arg =
     skipSpaces
  *> fmap Arg argName

addsub :: ReadP (ASTa a -> ASTa a -> ASTa a)
addsub = do
  skipSpaces
  c <- satisfy (`elem` "+-")
  case c of
    '+' -> return Add
    '-' -> return Sub
    _ -> pfail -- not reached

muldiv = do
  skipSpaces
  c <- satisfy (`elem` "*/")
  case c of
    '*' -> return Mul
    '/' -> return Div
    _ -> pfail -- not reached

factor = chainl1 imm muldiv

expr = chainl1 factor addsub

data Function a = Function [String] (ASTa a)
  deriving (Eq, Show)

cataAST :: Applicative f => (Int -> f (ASTa o)) -> (a -> f (ASTa o)) -> ASTa a -> f (ASTa o)
cataAST fImm _ (Imm x) = fImm x
cataAST _ fArg (Arg a) = fArg a
cataAST fImm fArg (Add x y) = Add <$> (cataAST fImm fArg x) <*> (cataAST fImm fArg y)
cataAST fImm fArg (Sub x y) = Sub <$> (cataAST fImm fArg x) <*> (cataAST fImm fArg y)
cataAST fImm fArg (Mul x y) = Mul <$> (cataAST fImm fArg x) <*> (cataAST fImm fArg y)
cataAST fImm fArg (Div x y) = Div <$> (cataAST fImm fArg x) <*> (cataAST fImm fArg y)

validateAST :: [String] -> ASTa String -> Either String AST
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
                                                        Right astInt -> Right $ Function args astInt

pass1 :: String -> AST
pass1 s = case parseExpr s of
            Right (Function _ result) -> result
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

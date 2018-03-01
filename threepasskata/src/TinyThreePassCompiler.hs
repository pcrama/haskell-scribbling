module TinyThreePassCompiler
    ( ASTa(..)
    , AST
    , compile
    , parseExpr
    , pass1
    , pass2
    , pass3
    ) where

import Data.Char (
  isDigit
  , isAlpha
  )

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

pass3 = undefined

compile = pass3 . pass2 . pass1

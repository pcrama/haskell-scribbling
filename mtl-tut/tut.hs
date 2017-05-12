-- I'm developing this in a restricted place without stack
-- and cabal -> manual step:
-- apt install libghc-mtl-dev # 2.1.2-4 armhf

--module Transformers where

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe
import qualified Data.Map as Map

type Name = String                     -- variable names

data Exp = Lit Integer                 -- expressions
         | Var Name
         | Plus Exp Exp
         | Abs Name Exp
         | App Exp Exp
         deriving (Show)

data Value = IntVal Integer            -- values 
           | FunVal Env Name Exp
           deriving (Show)

type Env = Map.Map Name Value

eval0 :: Env -> Exp -> Value
eval0 env (Lit i) = IntVal i
eval0 env (Var n) = fromJust $ Map.lookup n env
eval0 env (Plus e1 e2) = let IntVal v1 = eval0 env e1
                             IntVal v2 = eval0 env e2
                         in IntVal $ v1 + v2
eval0 env (Abs n e) = FunVal env n e
eval0 env (App q p) = let FunVal env' n' e' = eval0 env q
                          v = eval0 env p
                      in eval0 (Map.insert n' v env') e'

f1 = Abs "x" $ Abs "y" $ Plus (Lit 1)
                            $ Plus (Var "x") (Var "y")

f2 = Abs "z" $ App f1 (Var "z")

e1 = App (App f2 $ Lit 1) (Lit 2)

exampleExp = Lit 12 `Plus` (App (Abs "x" $ Var "x")
                                (Lit 4 `Plus` Lit 2))

main :: IO ()
main = do
  putStrLn . show $ e1
  putStrLn . show $ eval0 Map.empty e1
  putStrLn . show $ eval0 Map.empty exampleExp

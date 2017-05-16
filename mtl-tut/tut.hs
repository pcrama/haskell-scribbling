{-# LANGUAGE FlexibleContexts #-}
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

failExp = Lit 12 `Plus` (App (Abs "x" $ Var "y")
                             (Lit 4 `Plus` Lit 2))

failExp2 = Lit 12 `Plus` (Abs "x" $ Var "y")

type Eval1 a = Identity a

runEval1 :: Eval1 a -> a
runEval1 = runIdentity

eval1 :: Env -> Exp -> Eval1 Value
eval1 env (Lit i) = return $ IntVal i
eval1 env (Var n) =
  maybe (fail $ "Unknown variable " ++ n)
        return
      $ Map.lookup n env
eval1 env (Plus e1 e2) = do
  v1 <- eval1 env e1
  v2 <- eval1 env e2
  case (v1, v2) of
    (IntVal x1, IntVal x2) -> return . IntVal $ x1 + x2
    _ -> fail "At least one operand wasn't a number"
eval1 env (Abs n e) = return $ FunVal env n e
eval1 env (App q p) = do
  f <- eval1 env q
  case f of
    FunVal env' n' e' -> do
      v <- eval1 env p
      eval1 (Map.insert n' v env') e'
    _ -> fail "Can only apply something to function values"

type Eval2 a = ErrorT String Identity a
runEval2 = runIdentity . runErrorT

_getIntVal :: (MonadError String m) => m Value -> m Integer
-- _getIntVal :: Eval2 Value -> Eval2 Integer
_getIntVal = (>>= f)
  where f (IntVal x) = return x
        f (FunVal _ _ _) = throwError "Expected Int, got FunVal"

eval2a :: Env -> Exp -> Eval2 Value
eval2a env (Lit i) = return $ IntVal i
eval2a env (Var n) =
  maybe (throwError $ "Unknown variable " ++ n)
        return
      $ Map.lookup n env
eval2a env (Plus e1 e2) = do
  x1 <- _getIntVal $ eval2a env e1
  x2 <- _getIntVal $ eval2a env e2
  return . IntVal $ x1 + x2
eval2a env (Abs n e) = return $ FunVal env n e
eval2a env (App q p) = do
  f <- eval2a env q
  case f of
    FunVal env' n' e' -> do
      v <- eval2a env p
      eval2a (Map.insert n' v env') e'
    _ -> throwError "Can only apply something to function values"

type Eval3 a = ReaderT Env (ErrorT String Identity) a
runEval3 :: Env -> Eval3 a -> Either String a
runEval3 env = runIdentity . runErrorT . (flip runReaderT env)

eval3 :: Exp -> Eval3 Value
eval3 (Lit x) = return $ IntVal x
eval3 (Var n) = do
  env <- ask
  maybe (throwError $ "Unknown var " ++ n)
        return
      $ Map.lookup n env
eval3 (Plus e1 e2) = do
  x1 <- _getIntVal $ eval3 e1
  x2 <- _getIntVal $ eval3 e2
  return . IntVal $ x1 + x2
eval3 (Abs n e) = do
  env <- ask
  return $ FunVal env n e
eval3 (App q p) = do
  f <- eval3 q
  case f of
    FunVal env' n' e' -> do
      v <- eval3 p
      local (const $ Map.insert n' v env') $ eval3 e'
    _ -> throwError "Can only apply something to function values"

type StateVar = Int
-- ErrorT ... (StateT StateVar) -> Either an error or a
-- result, with final state in both cases
type Eval4 a = ReaderT Env (ErrorT String (StateT StateVar Identity)) a
runEval4 :: Env -> StateVar -> Eval4 a
         -> (Either String a, StateVar)
runEval4 env st = runIdentity
                . (flip runStateT st)
                . runErrorT
                . (flip runReaderT env)

tick :: (MonadState s m, Num s) => m ()
tick = get >>= put . (1+)

-- eval4 :: Exp -> Eval4 Value
eval4 (Lit x) = tick >> (return $ IntVal x)
eval4 (Var n) = do
  tick
  env <- ask
  maybe (throwError $ "Unknown var " ++ n)
        return
      $ Map.lookup n env
eval4 (Plus e1 e2) = do
  tick
  x1 <- _getIntVal $ eval4 e1
  x2 <- _getIntVal $ eval4 e2
  return . IntVal $ x1 + x2
eval4 (Abs n e) = do
  tick
  env <- ask
  return $ FunVal env n e
eval4 (App q p) = do
  tick
  f <- eval4 q
  case f of
    FunVal env' n' e' -> do
      v <- eval4 p
      local (const $ Map.insert n' v env') $ eval4 e'
    _ -> throwError "Can only apply something to function values"

-- StateT StateVar (ErrorT ...) -> Either an error or a
-- (result + final state)
type Eval4b a = ReaderT Env (StateT StateVar (ErrorT String Identity)) a
runEval4b :: Env -> StateVar -> Eval4b a
          -> Either String (a, StateVar)
runEval4b env st = runIdentity
                 . runErrorT
                 . (flip runStateT st)
                 . (flip runReaderT env)

type Eval5 a = ReaderT Env (ErrorT String (WriterT [String] (StateT StateVar Identity))) a
runEval5 :: Env -> StateVar -> Eval5 a -> ((Either String a, [String]), StateVar)
runEval5 env st = runIdentity
                . (flip runStateT st)
                . runWriterT
                . runErrorT
                . (flip runReaderT env)

-- Shuffle transformers
type Eval5b a = ErrorT String (StateT StateVar (WriterT [String] (ReaderT Env Identity))) a
runEval5b :: Env -> StateVar -> Eval5b a -> ((Either String a, StateVar), [String])
runEval5b env st = runIdentity
                 . (flip runReaderT env)
                 . runWriterT
                 . (flip runStateT st)
                 . runErrorT

-- eval5 :: Exp -> Eval5 Value
eval5 (Lit x) = tick >> (return $ IntVal x)
eval5 (Var n) = do
  tick
  tell [n]
  env <- ask
  maybe (throwError $ "Unknown var " ++ n)
        return
      $ Map.lookup n env
eval5 (Plus e1 e2) = do
  tick
  x1 <- _getIntVal $ eval5 e1
  x2 <- _getIntVal $ eval5 e2
  return . IntVal $ x1 + x2
eval5 (Abs n e) = do
  tick
  env <- ask
  return $ FunVal env n e
eval5 (App q p) = do
  tick
  f <- eval5 q
  case f of
    FunVal env' n' e' -> do
      v <- eval5 p
      local (const $ Map.insert n' v env') $ eval5 e'
    _ -> throwError "Can only apply something to function values"

type Eval6 a = ReaderT Env (ErrorT String
                           (WriterT [String]
                           (StateT StateVar IO))) a
runEval6 :: Env -> StateVar -> Eval6 a
         -> IO ((Either String a, [String]) , StateVar)
runEval6 env st = (flip runStateT st)
                . runWriterT
                . runErrorT
                . (flip runReaderT env)

-- eval5 :: Exp -> Eval5 Value
eval6 (Lit x) = do
  tick
  liftIO . putStrLn $ "Lit " ++ show x
  return $ IntVal x
eval6 (Var n) = do
  tick
  tell [n]
  env <- ask
  maybe (throwError $ "Unknown var " ++ n)
        return
      $ Map.lookup n env
eval6 (Plus e1 e2) = do
  tick
  x1 <- _getIntVal $ eval6 e1
  x2 <- _getIntVal $ eval6 e2
  return . IntVal $ x1 + x2
eval6 (Abs n e) = do
  tick
  env <- ask
  return $ FunVal env n e
eval6 (App q p) = do
  tick
  f <- eval6 q
  case f of
    FunVal env' n' e' -> do
      v <- eval6 p
      local (const $ Map.insert n' v env') $ eval6 e'
    _ -> throwError "Can only apply something to function values"

main :: IO ()
main = do
  putStrLn . show $ eval0 Map.empty e1
  putStrLn . show . runEval1 $ eval1 Map.empty e1
  putStrLn . show . runEval2 $ eval2a Map.empty e1
  putStrLn . show $ eval0 Map.empty exampleExp
  putStrLn . show . runEval1 $ eval1 Map.empty exampleExp
  putStrLn . show . runEval2 $ eval2a Map.empty exampleExp
  putStrLn . show . runEval2 $ eval2a Map.empty failExp
  putStrLn . show . runEval2 $ eval2a Map.empty failExp2
  putStrLn . show . runEval3 Map.empty $ eval3 exampleExp
  putStrLn . show . runEval3 Map.empty $ eval3 failExp
  putStrLn . show . runEval3 Map.empty $ eval3 failExp2
  putStrLn . show . runEval4 Map.empty 0 $ eval4 exampleExp
  putStrLn . show . runEval4 Map.empty 0 $ eval4 failExp
  putStrLn . show . runEval4 Map.empty 0 $ eval4 failExp2
  putStrLn . show . runEval4b Map.empty 0 $ eval4 exampleExp
  putStrLn . show . runEval4b Map.empty 0 $ eval4 failExp
  putStrLn . show . runEval4b Map.empty 0 $ eval4 failExp2
  putStrLn . show . runEval5 Map.empty 0 $ eval5 exampleExp
  putStrLn . show . runEval5 Map.empty 0 $ eval5 failExp
  putStrLn . show . runEval5 Map.empty 0 $ eval5 failExp2
  putStrLn . show . runEval5b Map.empty 0 $ eval5 exampleExp
  putStrLn . show . runEval5b Map.empty 0 $ eval5 failExp
  putStrLn . show . runEval5b Map.empty 0 $ eval5 failExp2
  (runEval6 Map.empty 0 $ eval6 exampleExp) >>= putStrLn . show
  (runEval6 Map.empty 0 $ eval6 failExp) >>= putStrLn . show
  (runEval6 Map.empty 0 $ eval6 failExp2) >>= putStrLn . show

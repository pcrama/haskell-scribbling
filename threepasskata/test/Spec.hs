-- stack exec --package QuickCheck -- ghci test/Spec.hs
import Test.Hspec
import Test.HUnit
import Test.QuickCheck
import Test.Hspec.Core.QuickCheck (modifyMaxSize, modifyMaxSuccess)

import TinyThreePassCompiler (AST(..), compile, pass1, pass2, pass3, simulate)

import Control.Applicative ((<$>), (<*>))
import Data.List           (foldl')
import Debug.Trace (trace)

ex1_prog = "[ xx yy ] ( xx + yy ) / 2"
-- ex1_prog = "[xx yy](xx+yy) / 2"
ex1_pass1 = Div (Add (Arg 0) (Arg 1)) (Imm 2)

ex2_prog = "[ x ] x + 2 * 5"
ex2_pass1 = Add (Arg 0) (Mul (Imm 2) (Imm 5))
ex2_pass2 = Add (Arg 0) (Imm 10)

ex3_prog = "[ a b ] a*a + b*b"
ex3_pass1 = Add (Mul (Arg 0) (Arg 0)) (Mul (Arg 1) (Arg 1))

traceId x = trace (show x) x

newtype OneMulArg = OneMulArg AST
  deriving (Show, Eq)

hasZeroArg (Imm _) = True
hasZeroArg (Arg _) = False
hasZeroArg (Add lft rgt) = hasZeroArg lft && hasZeroArg rgt
hasZeroArg (Sub lft rgt) = hasZeroArg lft && hasZeroArg rgt
hasZeroArg (Mul lft rgt) = hasZeroArg lft && hasZeroArg rgt
hasZeroArg (Div lft rgt) = hasZeroArg lft && hasZeroArg rgt

isOneMulArg (Imm _) = False
isOneMulArg (Arg _) = True
isOneMulArg (Add _ _) = False
isOneMulArg (Sub _ _) = False
isOneMulArg (Mul lft rgt) =
  hasZeroArg lft && isOneMulArg rgt || isOneMulArg lft && hasZeroArg rgt
isOneMulArg (Div _ _) = False

makeOneMulArg x | isOneMulArg x = Just $ OneMulArg x
                | otherwise = Nothing

instance Arbitrary OneMulArg where
  arbitrary = sized $ \size ->
    case size of
      0 -> oneof $ [fmap (OneMulArg . Imm) $ elements [-1, 0, 2], elements [OneMulArg $ Arg 0]]
      _ -> do
        lftSize <- choose (0, size)
        let rgtSize = max 0 $ size - lftSize - 1
        OneMulArg lft <- resize lftSize arbitrary
        OneMulArg rgt <- resize rgtSize arbitrary
        case makeOneMulArg $ Mul lft rgt of
          Nothing -> return $ OneMulArg lft
          Just x -> return x
  shrink (OneMulArg (Imm _)) = []
  shrink (OneMulArg (Arg _)) = [OneMulArg $ Imm 2]
  shrink (OneMulArg (Add lft rgt)) = map OneMulArg [lft, rgt, Add rgt lft, Add lft $ Imm 3, Add (Imm 4) rgt]
  shrink (OneMulArg (Sub lft rgt)) = map OneMulArg [lft, rgt, Sub rgt lft, Sub lft $ Imm 5, Sub (Imm 6) rgt, Add lft rgt]
  shrink (OneMulArg (Mul lft rgt)) = map OneMulArg [lft, rgt, Mul rgt lft, Mul lft $ Imm 7, Mul (Imm 8) rgt]
  shrink (OneMulArg (Div lft rgt)) = map OneMulArg [lft, rgt, Div rgt lft, Div lft $ Imm 9, Div (Imm 10) rgt, Mul lft rgt]

prop_reducesOneMulArgToMinimum (OneMulArg x) =
  let p = pass2 x
      arg = 123
  in (((==) <$> evalAST p <*> evalAST x $ [arg])
      && case pass2 x of
           Imm _ -> True
           Arg _ -> True
           Mul (Imm _) (Arg _) -> True
           Mul (Arg _) (Imm _) -> True
           _ -> False)

evalAST (Imm x) = return x
evalAST (Arg n) = \e -> e !! n
evalAST (Add x y) = (+) <$> evalAST x <*> evalAST y
evalAST (Sub x y) = (-) <$> evalAST x <*> evalAST y
evalAST (Mul x y) = (*) <$> evalAST x <*> evalAST y
evalAST (Div x y) = div <$> evalAST x <*> evalAST y

-- xxx = sample (arbitrary :: Gen OneMulArg)

main :: IO ()
main = hspec $ do
  describe "pass1" $ do
    it "should work for trivial examples" $ do
      "[] 1" `shouldPass1` (Imm 1)
      "[] 1 - 2" `shouldPass1` (Sub (Imm 1) (Imm 2))

    it "should work with variables" $ do
      "[x] x" `shouldPass1` (Arg 0)
      "[x y z] z" `shouldPass1` (Arg 2)
      "[ x y z] z + 1" `shouldPass1` (Add (Arg 2) (Imm 1))

    describe "should work for the examples" $ do
      it ex1_prog $ do
        ex1_prog `shouldPass1` ex1_pass1
      it ex2_prog $ do
        ex2_prog `shouldPass1` ex2_pass1
      it ex3_prog $ do
        ex3_prog `shouldPass1` ex3_pass1

  describe "pass2" $ do
    it "should work for the example" $ do
      ex2_pass1 `shouldPass2` ex2_pass2

    it "should handle arguments in correct order" $ do
      (Sub (Imm 10) (Imm 2)) `shouldPass2` (Imm 8)
      (Div (Imm 10) (Imm 2)) `shouldPass2` (Imm 5)

    it "should handle examples from attempt" $ do
      (Div (Sub (Add (Mul (Mul (Imm 2) (Imm 3)) (Arg 0)) (Mul (Imm 5) (Arg 1))) (Mul (Imm 3) (Arg 2)))
           (Add (Add (Imm 1) (Imm 3)) (Mul (Imm 2) (Imm 2)))) `shouldPass2` (
        Div (Sub (Add (Mul (Imm 6) (Arg 0)) (Mul (Imm 5) (Arg 1))) (Mul (Imm 3) (Arg 2))) (Imm 8))

    it "should handle counterexample found with QuickCheck property" $ do
      (Mul (Mul (Mul (Mul (Imm 1) (Arg 0)) (Imm 1)) (Imm (-1))) (Imm (-1))) `shouldPass2` (Arg 0)

    modifyMaxSize (const 1000) $ modifyMaxSuccess (const 10000) $ it "minimizes Mul as far as it can" $ do
      property prop_reducesOneMulArgToMinimum

  describe "pass3" $ do
    it "should work for the examples" $ do
      (ex1_pass1, [10, 8]) `shouldPass3` 9
      (ex2_pass2, [3]) `shouldPass3` 13

-- helper assertions to make more helpful error messages by including function inputs as well

shouldPass1 = assertPass pass1
shouldPass2 = assertPass pass2

assertPass f ast expected = assertEqual errorMsg expected (f ast)
  where errorMsg = unlines ["input: " ++ show ast]

shouldPass3 (ast, args) expected = assertEqual errorMsg expected $ simulate (pass3 ast) args
  where errorMsg = unlines ["simulate (pass3 " ++ show ast ++ ") " ++ show args]

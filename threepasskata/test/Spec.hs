import Test.Hspec
import Test.HUnit
import Test.QuickCheck
import TinyThreePassCompiler (ASTa (..), AST, compile, pass1, pass2, pass3, simulate)

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

module Act2Scene2 (
  spec
)
where

import Semiring
import SpecHelper
import LibAct2

testSubmatchW :: (Show a, Eq a, Show s, Eq s, Semiring s)
              => Reg s (Int, a) -> [a] -> s -> SpecWith ()
testSubmatchW r as expected =
  it ((if (expected == zero) then "rejec" else "accep") ++ "ts " ++ show as) $
    submatchW r as `shouldBe` expected

symC :: Semiring s => Char -> Reg s Char
symC c = symS $ \x -> if x == c then one else zero

spec :: Spec
spec = describe "A Play on Regular Expressions, Act 2 Scene 2" $ do
  context "has a function submatchW that" $ do
    context "works on an example" $
      let test = testSubmatchW (symS $ \(i, c) -> case i `mod` 2 of
                                                    0 -> c == '0'
                                                    1 -> c == '1'
                                                    _ -> error "new math: `mod` 2 returned neither 0, nor 1" ) in do
        test "" False
        test "0" True
        test "1" False
        test "abc" False
        test "a01bc" False
        test "a00a" True

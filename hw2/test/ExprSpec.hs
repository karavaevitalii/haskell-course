module ExprSpec where

import           MonadicCalc
import           Test.Hspec

spec :: Spec
spec = do
  it "expression" $ do
    eval (Const 42) `shouldBe` Right 42
    eval (Add (Const 5) (Mul (Const 2) (Const 3))) `shouldBe` Right (5 + 2 * 3)
    eval (Sub (Div (Const 4) (Const 2)) (Const 2)) `shouldBe` Right (4 `div` 2 - 2)
    eval (Div (Const 3) (Const 0)) `shouldBe` Left DivisionByZero
    eval (Pow (Const 3) (Const (-1))) `shouldBe` Left NegativeExponent
    eval (Add (Const 5) (Div (Const 6) (Sub (Const 2) (Const 2)))) `shouldBe` Left DivisionByZero
    eval (Pow (Const 42) (Sub (Const 1) (Const 2))) `shouldBe` Left NegativeExponent

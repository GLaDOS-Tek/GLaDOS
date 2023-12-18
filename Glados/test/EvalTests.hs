module EvalTests (evalTests) where

import Test.Hspec
import Eval (evalAst)
import Structs (Ast(..))

evalTests :: IO ()
evalTests = hspec $ do
  describe "Evaluate AST Expressions" $ do
    it "should evaluate number to 42" $ do
      evalAst [] (Number 42) `shouldBe` ([], [Number 42])
    it "should evaluate addition to 84" $ do
      evalAst [] (Call "+" [Number 42, Number 42]) `shouldBe` ([], [Number 84])
    it "should evaluate to substraction to 56" $ do
      evalAst [] (Call "-" [Number 64, Number 8]) `shouldBe` ([], [Number 56])
    it "should evaluate to bool equality to True" $ do
      evalAst [] (Call "=" [Boolean True, Boolean True]) `shouldBe` ([], [Boolean True])
    it "should evaluate inequality to True" $ do
      evalAst [] (Call "<" [Number 100, Number 58]) `shouldBe` ([], [Boolean True])
    it "should evaluate define to updated env" $ do
      evalAst [] (Define "x" (Number 42)) `shouldBe` ([("x", Number 42)], [])
    it "should evaluate get variable to 42" $ do
      evalAst [] (AstList [Define "x" (Number 42), Symbol "x"]) `shouldBe` ([("x", Number 42)], [Number 42])
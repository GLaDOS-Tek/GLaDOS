module AstTests (astTests) where

import Test.Hspec
import Eval (sexprToAST)
import Structs (SExpr(..), Ast(..))

astTests :: IO ()
astTests = hspec $ do
  describe "SExpr to Ast Conversion" $ do
    it "should convert an integer expression" $ do
      sexprToAST (IntExpr 42 1) `shouldBe` Number 42

    -- it "should convert a string expression" $ do
    --   sexprToAST (StrExpr "Hello" 1) `shouldBe` String "Hello"

    -- it "should convert a symbol expression" $ do
    --   sexprToAST (SymbolExpr "symbol" 1) `shouldBe` Symbol "symbol"

    -- it "should convert an empty list expression" $ do
    --   sexprToAST (ExprList []) `shouldBe` AstList []

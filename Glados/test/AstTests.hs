module AstTests (astTests) where

import Test.Hspec
import Structs 

astTests :: IO ()
astTests = putStrLn "AstTest"
  -- hspec $ do
  -- describe "SExpr to Ast Conversion" $ do
  --   it "should convert an integer expression" $ do
  --     sexprToAST (IntExpr 42 1) `shouldBe` Number 42

  --   it "should convert a string expression" $ do
  --     sexprToAST (StrExpr "Hello" 1) `shouldBe` Str "Hello"

  --   it "should convert a symbol expression" $ do
  --     sexprToAST (SymbolExpr "symbol" 1) `shouldBe` Symbol "symbol"

  --   it "should convert an empty list expression" $ do
  --     sexprToAST (ExprList []) `shouldBe` AstList []

  --   it "should convert a define expression" $ do
  --     sexprToAST (SymbolExpr "define" 1) `shouldBe` Symbol "define"

  --   it "should convert a defun expression" $ do
  --     sexprToAST (SymbolExpr "defun" 1) `shouldBe` Symbol "defun"

  --   it "should convert a if expression" $ do
  --     sexprToAST (SymbolExpr "if" 1) `shouldBe` Symbol "if"

  --   it "handle error case of define symbol" $ do
  --     let invalidDefineExpr = ExprList [SymbolExpr "define" 1, IntExpr 42 2]
  --     sexprToAST invalidDefineExpr `shouldBe` Error "missing symbol or value to define statement" 1

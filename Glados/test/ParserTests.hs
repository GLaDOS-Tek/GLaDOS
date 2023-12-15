module ParserTests (parserTests) where

import Test.Hspec
import Parser (generalParser, SExpr(..))

parserTests :: IO ()
parserTests = hspec $ do
  describe "Parsing expressions" $ do
    it "should parse an integer expression" $ do
      generalParser 1 "42" `shouldBe` Just (IntExpr 42 1, "")

    it "should parse a string expression" $ do
      generalParser 1 "\"Hello\"" `shouldBe` Just (StrExpr "Hello" 1, "")

    it "should parse a symbol expression" $ do
      generalParser 1 "symbol" `shouldBe` Just (SymbolExpr "symbol" 1, "")

    it "should parse an empty list expression" $ do
      generalParser 1 "()" `shouldBe` Just (ExprList [], "")

    it "should parse a list expression with elements" $ do
      generalParser 1 "(1 \"hello\" symbol)" `shouldBe`
        Just (ExprList [IntExpr 1 1, StrExpr "hello" 1, SymbolExpr "symbol" 1], "")

    it "should handle newlines and tabs while parsing" $ do
      generalParser 1 "(\n\t42\n)" `shouldBe` Just (ExprList [IntExpr 42 2], "")

    it "should handle empty input" $ do
      generalParser 1 "" `shouldBe` Nothing

    it "should fail to parse invalid input" $ do
      generalParser 1 "abcde" `shouldBe` Just (SymbolExpr "abcde" 1,"")
module ParserTests (parserTests) where

-- IMPORTS

import Test.Hspec
import Parser (generalParser, exprParser, parseString, parseList, parseInt, parseSome, parseMany, parseAnyChar, parseChar)
import Structs (SExpr(..))

-- CALL ALL TESTS

parserTests :: IO ()
parserTests =
  testParseAnyChar >>
  testParseMany >>
  testParseSome >>
  testParseInt >>
  testParseList >>
  testParseString >>
  testExprParser >>
  testGeneralParser

-- TESTS

testParseChar :: IO ()
testParseChar = hspec $ do
  describe "Tests for parseChar" $ do
    it "should parse a number from string" $ do
      parseChar '4' "404bob" `shouldBe` Just ('4', "04bob")

    it "should parse a char" $ do
      parseChar 'a' "abcdefg" `shouldBe` Just ('a', "bcdefg")

    it "should fail to parse 1" $ do
      parseChar '4' "bob" `shouldBe` Nothing

    it "should fail to parse 2" $ do
      parseChar '4' "" `shouldBe` Nothing

testParseAnyChar :: IO ()
testParseAnyChar = hspec $ do
  describe "Tests for parseAnyChar" $ do
    it "should parse a number from string" $ do
      parseAnyChar ['0'..'9'] "404bob" `shouldBe` Just ('4', "04bob")

    it "should parse a char" $ do
      parseAnyChar ['a'..'e'] "abcdefg" `shouldBe` Just ('a', "bcdefg")

    it "should fail to parse 1" $ do
      parseAnyChar ['0'..'9'] "bob" `shouldBe` Nothing

    it "should fail to parse 2" $ do
      parseAnyChar ['0'..'9'] "" `shouldBe` Nothing

testParseMany :: IO ()
testParseMany = hspec $ do
  describe "Tests for parseMany" $ do
    it "should parse a number from string" $ do
      parseMany (parseAnyChar ['0'..'9']) "404bob" `shouldBe` Just ("404", "bob")

    it "should parse a string" $ do
      parseMany (parseAnyChar ['a'..'e']) "abcdefg" `shouldBe` Just ("abcde", "fg")

    it "shouldn't find something 1" $ do
      parseMany (parseAnyChar ['0'..'9']) "bob" `shouldBe` Just ("", "bob")

    it "shouldn't find something 2" $ do
      parseMany (parseAnyChar ['0'..'9']) "" `shouldBe` Just ("", "")

testParseSome :: IO ()
testParseSome = hspec $ do
  describe "Tests for parseSome" $ do
    it "should parse a number from string" $ do
      parseSome (parseAnyChar ['0'..'9']) "404bob" `shouldBe` Just ("404", "bob")

    it "shouldn't find a missing char" $ do
      parseSome (parseChar 'a') "bc" `shouldBe` Nothing

testParseInt :: IO ()
testParseInt = hspec $ do
  describe "Tests for parseInt" $ do
    it "should parse an integer" $ do
      parseInt "42" `shouldBe` Just (42, "")

    it "should fail to parse an invalid integer 1" $ do
      parseInt "bob" `shouldBe` Nothing

    it "should fail to parse an invalid integer 2" $ do
      parseInt "42bob" `shouldBe` Nothing

testParseList :: IO ()
testParseList = hspec $ do
  describe "Tests for parseList" $ do
    it "should parse an empty list" $ do
      parseList 1 "()" `shouldBe` Just ([], "")

    it "should parse a list with elements" $ do
      parseList 1 "(1 \"hello\" symbol)" `shouldBe`
        Just ([IntExpr 1 1, StrExpr "hello" 1, SymbolExpr "symbol" 1], "")

    it "should fail to parse an invalid list 1" $ do
      parseList 1 "1 \"hello\" symbol)" `shouldBe` Nothing

    it "should fail to parse an invalid list 2" $ do
      parseList 1 "(1 \"hello\" symbol" `shouldBe` Nothing

testParseString :: IO ()
testParseString = hspec $ do
  describe "Tests for parseString" $ do
    it "should parse a string" $ do
      parseString 1 "\"Hello\"" `shouldBe` Just (StrExpr "Hello" 1, "")

    it "should parse a symbol" $ do
      parseString 1 "symbol" `shouldBe` Just (SymbolExpr "symbol" 1, "")

    it "should fail to parse an invalid string" $ do
      parseString 1 "\"Hello" `shouldBe` Nothing

testExprParser :: IO ()
testExprParser = hspec $ do
  describe "Tests for exprParser" $ do
    it "should parse an integer expression" $ do
      exprParser 1 "42" `shouldBe` Just (IntExpr 42 1, "")

    it "should parse a string expression" $ do
      exprParser 1 "\"Hello\"" `shouldBe` Just (StrExpr "Hello" 1, "")

    it "should parse a symbol expression" $ do
      exprParser 1 "symbol" `shouldBe` Just (SymbolExpr "symbol" 1, "")

    it "should parse a list expression with elements" $ do
      exprParser 1 "(1 \"hello\" symbol)" `shouldBe`
        Just (ExprList [IntExpr 1 1, StrExpr "hello" 1, SymbolExpr "symbol" 1], "")

    it "should fail to parse invalid input" $ do
      exprParser 1 "\"abcde" `shouldBe` Nothing

testGeneralParser :: IO ()
testGeneralParser = hspec $ do
  describe "Tests for generalParser" $ do
    it "should parse an integer expression" $ do
      generalParser 1 "42" `shouldBe` Just ([IntExpr 42 1], "")

    it "should parse a string expression" $ do
      generalParser 1 "\"Hello\"" `shouldBe` Just ([StrExpr "Hello" 1], "")

    it "should parse a symbol expression" $ do
      generalParser 1 "symbol" `shouldBe` Just ([SymbolExpr "symbol" 1], "")

    it "should parse an empty list expression" $ do
      generalParser 1 "()" `shouldBe` Just ([ExprList []], "")

    it "should parse a list expression with elements" $ do
      generalParser 1 "(1 \"hello\" symbol)" `shouldBe`
        Just ([ExprList [IntExpr 1 1, StrExpr "hello" 1, SymbolExpr "symbol" 1]], "")

    it "should handle newlines and tabs while parsing" $ do
      generalParser 1 "(\n\t42\n)" `shouldBe` Just ([ExprList [IntExpr 42 2]], "")

    it "should handle empty input" $ do
      generalParser 1 "" `shouldBe` Just ([], "")

    it "should fail to parse invalid input" $ do
      generalParser 1 "abcde" `shouldBe` Just ([SymbolExpr "abcde" 1],"")
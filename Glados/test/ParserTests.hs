module ParserTests (
  parserTests
) where

-- IMPORTS

import Test.Hspec
import Parser

-- CALL ALL TESTS

parserTests :: IO ()
parserTests =
  testIsWhitespace >>
  testParseWS >>
  testParseChar >>
  testParseAnyChar >>
  testParseMany >>
  testParseSome >>
  testParseUInt >>
  testParseInt >>
  testParseString >>
  testParseSymbol >>
  testParseWord >>
  testParseBoolean
-- TESTS

testIsWhitespace :: IO ()
testIsWhitespace = hspec $ do
  describe "Test for isWhitespace" $ do
    it "should return True" $ do
      isWhitespace ' ' `shouldBe` True
    it "should return True" $ do
      isWhitespace '\n' `shouldBe` True
    it "should return True" $ do
      isWhitespace '\t' `shouldBe` True
    it "should return False" $ do
      isWhitespace 'a' `shouldBe` False

testParseWS :: IO ()
testParseWS = hspec $ do
  describe "Test for parseWS" $ do
    it "should parse whitespaces until none left" $ do
      runParser parseWS "         hello" `shouldBe` Just ("         ", "hello")
    it "should parse whitespaces until none left" $ do
      runParser parseWS "     \t\n   \n hello" `shouldBe` Just ("     \t\n   \n ", "hello")
    it "should no parse anything" $ do
      runParser parseWS "hello" `shouldBe` Just ("", "hello")

testParseChar :: IO ()
testParseChar = hspec $ do
  describe "Tests for parseChar" $ do
    it "should parse a number from string" $ do
      runParser (parseChar '4') "404bob" `shouldBe` Just ('4', "04bob")

    it "should parse a char" $ do
       runParser (parseChar 'a') "abcdefg" `shouldBe` Just ('a', "bcdefg")

    it "should fail to parse 1" $ do
      runParser (parseChar '4') "bob" `shouldBe` Nothing

    it "should fail to parse 2" $ do
      runParser (parseChar '4') "" `shouldBe` Nothing

testParseAnyChar :: IO ()
testParseAnyChar = hspec $ do
  describe "Tests for parseAnyChar" $ do
    it "should parse a number from string" $ do
      runParser  (parseAnyChar ['0'..'9']) "404bob" `shouldBe` Just ('4', "04bob")

    it "should parse a char" $ do
      runParser (parseAnyChar ['a'..'e']) "abcdefg" `shouldBe` Just ('a', "bcdefg")

    it "should fail to parse 1" $ do
      runParser  (parseAnyChar ['0'..'9']) "bob" `shouldBe` Nothing

    it "should fail to parse 2" $ do
      runParser  (parseAnyChar ['0'..'9']) "" `shouldBe` Nothing

testParseMany :: IO ()
testParseMany = hspec $ do
  describe "Tests for parseMany" $ do
    it "should parse a number from string" $ do
      runParser (parseMany (parseAnyChar ['0'..'9'])) "404bob" `shouldBe` Just ("404", "bob")

    it "should parse a string" $ do
      runParser (parseMany (parseAnyChar ['a'..'e'])) "abcdefg" `shouldBe` Just ("abcde", "fg")

    it "shouldn't find something 1" $ do
      runParser (parseMany (parseAnyChar ['0'..'9'])) "bob" `shouldBe` Just ("", "bob")

    it "shouldn't find something 2" $ do
      runParser (parseMany (parseAnyChar ['0'..'9'])) "" `shouldBe` Just ("", "")

    it "should parse only 1 element" $ do
      runParser (parseMany (parseAnyChar ['0'..'9'])) "1abc" `shouldBe` Just ("1", "abc")

testParseSome :: IO ()
testParseSome = hspec $ do
  describe "Tests for parseSome" $ do
    it "should parse a number from string" $ do
      runParser (parseSome (parseAnyChar ['0'..'9'])) "404bob" `shouldBe` Just ("404", "bob")

    it "shouldn't find a missing char" $ do
      runParser (parseSome (parseChar 'a')) "bc" `shouldBe` Nothing

testParseUInt :: IO ()
testParseUInt = hspec $ do
  describe "Tests for parseUInt" $ do
    it "should parse an integer" $ do
      runParser parseUInt "42" `shouldBe` Just (42, "")

    it "should fail to parse an invalid integer 1" $ do
      runParser parseUInt "bob" `shouldBe` Nothing

    it "should fail to parse an invalid integer 2" $ do
      runParser parseUInt "42bob" `shouldBe` Nothing
    it "should fail to parse anything" $ do
      runParser parseUInt "%$$'" `shouldBe` Nothing

testParseInt :: IO ()
testParseInt = hspec $ do
  describe "Tests for parseInt" $ do
    it "should parse an integer" $ do
      runParser parseInt "42" `shouldBe` Just (42, "")

    it "should parse a negative integer" $ do
      runParser parseInt "-42" `shouldBe` Just (-42, "")

    it "should fail to parse an invalid integer 1" $ do
      runParser parseInt "bob" `shouldBe` Nothing

    it "should fail to parse an invalid integer 2" $ do
      runParser parseInt "42bob" `shouldBe` Nothing

    it "should fail to parse an invalid integer 2" $ do
      runParser parseInt "-a42bob" `shouldBe` Nothing

testParseString :: IO ()
testParseString = hspec $ do
  describe "Tests for parseString" $ do
    it "should parse a string" $ do
      runParser parseString "\"Hello\"" `shouldBe` Just ("Hello", "")

    it "should not parse a string" $ do
      runParser parseString "\"Hi guys !\" rest of the code..." `shouldBe` Just ("Hi guys !", " rest of the code...")

    it "should not parse a symbol" $ do
      runParser parseString "symbol" `shouldBe` Nothing

    it "should fail to parse an invalid string" $ do
      runParser parseString "\"Hello" `shouldBe` Nothing

testParseSymbol :: IO()
testParseSymbol = hspec $ do
  describe "Tests for parseSymbol" $ do
    it "should parse a symbol" $ do
      runParser parseSymbol "feed a 10" `shouldBe` Just ("feed", " a 10")
    it "should not parse a symbol" $ do
      runParser parseSymbol "= a 10" `shouldBe` Nothing

testParseWord :: IO()
testParseWord = hspec $ do
  describe "Tests for parseWord" $ do
    it "should parse a specific keyword" $ do
      runParser (parseWord "feed") "feed a 10" `shouldBe` Just ("feed", " a 10")
    it "should fail to parse a specific keyword" $ do
      runParser (parseWord "feed") "cat a 10" `shouldBe` Nothing

testParseBoolean :: IO()
testParseBoolean = hspec $ do
  describe "Tests for parseBoolean" $ do
    it "should parse a True boolean" $ do
      runParser parseBoolean "true 1234" `shouldBe` Just (True, " 1234")
    it "should parse a False boolean" $ do
      runParser parseBoolean "false 1234" `shouldBe` Just (False, " 1234")
    it "should fail to parse a boolean" $ do
      runParser parseBoolean "feed a 10" `shouldBe` Nothing

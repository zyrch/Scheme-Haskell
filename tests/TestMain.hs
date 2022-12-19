module Main where

import Scheme.Parser.ParseString
import Scheme.Parser.ParseExpr
import Scheme.Parser.LispVal
import Scheme.Evaluator

import Text.ParserCombinators.Parsec hiding (spaces)

import Test.Tasty.HUnit
import Test.Tasty

-- testParseCharacter = TestCase $ assertEqual "Parsing #\\A" (Right 'A') (parse parseCharacter "char" "#\\A")
-- testParseCharacter = TestCase $ assertEqual "Parsing #\\A" (Right 'A') (parse parseExpr "lisp" "string") 

checkEquality :: (Eq a) => a -> Either ParseError a -> Bool
checkEquality expected input = case input of
                        Left err -> False
                        Right val -> (val == expected)


testCharacters = testGroup "Parsing Characters"
    [ 
      -- correct parsing characters
      testCase "Parsing 'A'" $
      assertBool "" $ checkEquality (Character 'A') (parse parseExpr "char" "#\\A")
    , testCase "Parsing ' '" $
      assertBool "" $ checkEquality (Character ' ') (parse parseExpr "char" "#\\ ")
    , testCase "Parsing 'b'" $
      assertBool "" $ checkEquality (Character 'b') (parse parseExpr "char" "#\\b")
    , testCase "Parsing '*'" $
      assertBool "" $ checkEquality (Character '*') (parse parseExpr "char" "#\\*")

      -- correctly not parsing others
    , testCase "Not Parsing A" $
      assertEqual "" False $ checkEquality (Character 'A') (parse parseExpr "Atom" "A")
    , testCase "Not Parsing \"(\"" $
      assertEqual "" False $ checkEquality (Character '(') (parse parseExpr "String" "\"(\"")
    ]

testStrings = testGroup "Parsing Strings"
    [
      -- should parse
      testCase "" $ assertBool "" $ checkEquality (String "this is a string") (parse parseExpr "str" "\"this is a string\"")
    , testCase "" $ assertBool "" $ checkEquality (String "\"string with backward slash") 
      (parse parseExpr "str" "\"\\\"string with backward slash\"")
    , testCase "" $ assertBool "" $ checkEquality (String "\nstring with escaped n") 
      (parse parseExpr "str" "\"\\nstring with escaped n\"")
    ]

testSymbols = testGroup "Parsing Atoms"
    [
      -- should parse
      testCase "" $ assertBool "" $ checkEquality (Atom "symbol") (parse parseExpr "atom" "symbol")
    , testCase "" $ assertBool "" $ checkEquality (Atom "symbol!") (parse parseExpr "atom" "symbol!")
    ]

tests :: TestTree
tests = testGroup "All Tests" [testCharacters, testStrings, testSymbols]

main = defaultMain tests

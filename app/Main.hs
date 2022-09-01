module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import System.Environment
import Data.Char(digitToInt, ord, intToDigit, toUpper)
import Scheme.Parser.ParseAtom
import Scheme.Parser.ParseString
import Scheme.Parser.LispVal
import Scheme.Parser.ParseNumber

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ "Parsing " ++ args !! 0
  putStrLn (readExpr $ args !! 0)

parseExpr :: Parser LispVal
parseExpr = parseCharacter
         <|> parseString 
         <|> parseNumber
         <|> parseAtom

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> "No Match: " ++ show err
                   Right val -> "Found Value: " ++ show val 



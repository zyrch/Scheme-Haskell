module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import System.Environment
import Data.Char(digitToInt, ord, intToDigit, toUpper)

import Scheme.Parser.ParseExpr
import Scheme.Parser.LispVal
import Scheme.Evaluator

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ "Parsing " ++ args !! 0
  putStrLn (readExpr $ args !! 0)

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> "No Match: " ++ show err
                   Right val -> "Found Value evaluations to: " ++ show val

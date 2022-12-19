module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import System.Environment
import Data.Char(digitToInt, ord, intToDigit, toUpper)

import Scheme.Parser.ParseExpr
import Scheme.Parser.LispVal
import Scheme.Evaluator

main :: IO ()
main = getArgs >>= print . eval . readExpr . head


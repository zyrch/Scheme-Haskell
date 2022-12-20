module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Control.Monad.Except
import System.Environment
import Data.Char(digitToInt, ord, intToDigit, toUpper)

import Scheme.Parser.ParseExpr
import Scheme.Parser.LispVal
import Scheme.Evaluator
import Scheme.LispError

main :: IO ()
main = do
  args <- getArgs >>= return . head
  print $ extractValue $ catchError ((readExpr args) >>= eval) (return . String . show)


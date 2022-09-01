module Scheme.Parser.ParseAtom 
( parseAtom
) where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Scheme.Parser.LispVal

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest  <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
             "#t" -> Bool True
             "#f" -> Bool False
             _    -> Atom atom

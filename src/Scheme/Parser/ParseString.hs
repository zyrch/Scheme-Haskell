module Scheme.Parser.ParseString
( parseString
, parseCharacter
) where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Scheme.Parser.LispVal


parseCharacter :: Parser LispVal
parseCharacter = liftM Character $ string "#\\" >> anyChar

parseEscapeSequence :: Parser Char
parseEscapeSequence = do
  char '\\'
  c <- oneOf "nr\\t\""     -- oneOf nrt\"
  return $ case c of
        '\\' -> c
        '"'  -> c
        'n'  -> '\n'
        'r'  -> '\r'
        't'  -> '\t'

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many $ noneOf "\\\"" <|> parseEscapeSequence 
  char '"'
  return $ String x



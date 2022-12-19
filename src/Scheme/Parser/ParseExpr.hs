module Scheme.Parser.ParseExpr
( parseExpr, readExpr
) where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Scheme.Parser.LispVal
import Scheme.Parser.ParseString
import Scheme.Parser.ParseNumber
import Scheme.Parser.ParseAtom

spaces :: Parser ()
spaces = skipMany1 space

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseCharacter
         <|> parseString 
         <|> parseNumber
         <|> parseAtom
         <|> parseQuoted
         <|> do char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> String $ "No Match: " ++ show err
                   Right val -> val

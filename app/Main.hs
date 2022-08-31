module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import System.Environment
import Data.Char(digitToInt, ord, intToDigit, toUpper)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             deriving Show

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ "Parsing " ++ args !! 0
  putStrLn (readExpr $ args !! 0)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

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

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest  <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
             "#t" -> Bool True
             "#f" -> Bool False
             _    -> Atom atom

baseToValidChars :: Int -> String
baseToValidChars base = if base <= 10 then "" 
                   else ['a' .. (toEnum $ ord 'a' + base - 11)]

parseNumberInBase :: Integer -> Parser LispVal
parseNumberInBase base = liftM (Number . toDecimal base) $ (many1 . oneOf) (map toUpper validAlphabets ++ validAlphabets ++ validDigits)
  where validAlphabets = (baseToValidChars . fromIntegral) base
        validDigits = ['0' .. toEnum $ fromEnum '0' + (min 9 (fromIntegral base - 1))]

parseNumber :: Parser LispVal
parseNumber = (try $ string "0b" >> parseNumberInBase 2)
           <|> (try $ string "0b" >> parseNumberInBase 8)
           <|> (try $ string "0x" >> parseNumberInBase 16)
           <|> parseNumberInBase 10

toDecimal :: Integer -> String -> Integer
toDecimal base s = foldl1 ((+) . (* base)) $ map (toInteger . digitToInt) s

parseExpr :: Parser LispVal
parseExpr = parseAtom 
         <|> parseString 
         <|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> "No Match: " ++ show err
                   Right val -> "Found Value: " ++ show val 



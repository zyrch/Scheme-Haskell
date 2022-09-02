module Scheme.Parser.ParseNumber
( parseNumber
) where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Data.Char(digitToInt, ord, intToDigit, toUpper)
import Scheme.Parser.LispVal

baseToValidChars :: Int -> String
baseToValidChars base = if base <= 10 then "" 
                   else ['a' .. (toEnum $ ord 'a' + base - 11)]

parseNumberInBase :: Integer -> Parser LispVal
parseNumberInBase base = liftM (Number . toDecimal base) $ (many1 . oneOf) (map toUpper validAlphabets ++ validAlphabets ++ validDigits)
  where validAlphabets = (baseToValidChars . fromIntegral) base
        validDigits = ['0' .. toEnum $ fromEnum '0' + (min 9 (fromIntegral base - 1))]

parseNumber :: Parser LispVal
parseNumber = (try $ string "0b" >> parseNumberInBase 2)
           <|> (try $ string "0o" >> parseNumberInBase 8)
           <|> (try $ string "0x" >> parseNumberInBase 16)
           <|> (try $ parseFloat)
           <|> parseNumberInBase 10

toDecimal :: Integer -> String -> Integer
toDecimal base s = foldl1 ((+) . (* base)) $ map (toInteger . digitToInt) s

parseFloat :: Parser LispVal
parseFloat = do
  integerPart <- many1 digit
  char '.'
  decimalPart <- many1 digit
  return $ (Float . read) $ integerPart ++ "." ++ decimalPart

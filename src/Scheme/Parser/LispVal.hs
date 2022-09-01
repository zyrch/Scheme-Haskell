module Scheme.Parser.LispVal where 

import Text.ParserCombinators.Parsec hiding (spaces)
import Data.Char(digitToInt, ord, intToDigit, toUpper)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             deriving Show


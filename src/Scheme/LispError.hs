module Scheme.LispError where

import Control.Monad.Except
import Scheme.Parser.LispVal
import Text.ParserCombinators.Parsec hiding (spaces)
import Data.List

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

type ThrowsError = Either LispError

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ func
showError (NumArgs expected found)      = "Expected " ++ show expected ++ 
                                          " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                          ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

unwordsList :: [LispVal] -> String
unwordsList = intercalate " " . map showVal

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Character contents) = [contents]
showVal (Float contents) = show contents
showVal (List list) = "( " ++ unwordsList list ++ " )"
showVal (DottedList head tail) = "( " ++ unwordsList head ++ " . " ++ showVal tail ++ " )"

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

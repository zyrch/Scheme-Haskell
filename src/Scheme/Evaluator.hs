module Scheme.Evaluator where

import Scheme.Parser.LispVal
import Data.List

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

unwordsList :: [LispVal] -> String
unwordsList = intercalate " " . map showVal

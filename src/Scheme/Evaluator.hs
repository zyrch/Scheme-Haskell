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

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args


apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop div),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op args = Number $ foldl1 op $ map unpackNum args

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum _ = 0



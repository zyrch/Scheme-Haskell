module Scheme.Primitives.Binops where

import Scheme.Parser.LispVal
import Scheme.LispError
import Control.Monad.Except

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop op args@[a, b] = mapM unpackStr args >>= (\[x, y] -> return . Bool $ op x y)
strBoolBinop op args        = throwError $ NumArgs 2 args

unpackStr :: LispVal -> ThrowsError String
unpackStr (String str) = return str
unpackStr notStr       = throwError $ TypeMismatch "string" notStr

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop op args@[a, b] = mapM unpackBool args >>= (\[x, y] -> return . Bool $ op x y)
boolBoolBinop op args        = throwError $ NumArgs 2 args

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "bool" notBool

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop op args@[a, b] = mapM unpackNum args >>= (\[x, y] -> return . Bool $ op x y)
numBoolBinop op args        = throwError $ NumArgs 2 args

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op []       = throwError $ NumArgs 2 []
numericBinop op args@[_] = throwError $ NumArgs 2 args
numericBinop op args     = mapM unpackNum args >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

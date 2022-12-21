module Scheme.Primitives.List where

import Scheme.Parser.LispVal
import Scheme.LispError
import Control.Monad.Except

car :: [LispVal] -> ThrowsError LispVal
car [List (x : _)]         = return x
car [DottedList (x : _) _] = return x
car [badType]              = throwError $ TypeMismatch "List" badType
car badArgList             = throwError $ NumArgs 1 badArgList


cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)]        = return $ List xs
cdr [DottedList [_] tl]    = return $ List [tl]
cdr [DottedList (_:xs) tl] = return $ List $ xs ++ [tl]
cdr [badType]              = throwError $ TypeMismatch "List" badType
cdr badArgList             = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List xs]               = return $ List $ x : xs
cons [x, DottedList head tail]  = return $ DottedList (x:head) tail
cons [x, y]                     = return $ DottedList [x] y
cons badArgList                 = throwError $ NumArgs 2 badArgList

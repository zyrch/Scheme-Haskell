module Scheme.Evaluator where

import Scheme.Parser.LispVal
import Scheme.LispError
import Control.Monad.Except

import Scheme.Primitives.Binops
import Scheme.Primitives.List

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val

-- TODO: Add cond and case expressions

eval (List [Atom "if", pred, conseq, alt]) = 
     do result <- evalPred pred
        case result of
             False -> eval alt
             True  -> eval conseq

eval (List (Atom func : args)) = (mapM eval args) >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

evalPred :: LispVal -> ThrowsError Bool
evalPred pred =
     do result <- eval pred
        case result of
            Bool False -> return $ False
            Bool True  -> return $ True
            otherwise  -> throwError $ TypeMismatch "predicate should be Bool" result

evalClause :: LispVal -> Maybe (ThrowsError LispVal)
evalClause (List [pred, conseq]) =
        case evalPred pred of
            Left  err   -> Just (Left err)
            Right True  -> Just (eval conseq)
            Right False -> Nothing

-- ThrowsError Bool

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive args" func ) ($ args) (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+",            numericBinop (+)),
              ("-",            numericBinop (-)),
              ("*",            numericBinop (*)),
              ("/",            numericBinop div),
              ("mod",          numericBinop div),
              ("quotient",     numericBinop quot),
              ("remainder",    numericBinop rem),
              ("=",            numBoolBinop (==)),
              ("<",            numBoolBinop (<)),
              (">",            numBoolBinop (>)),
              ("/=",           numBoolBinop (/=)),
              (">=",           numBoolBinop (>=)),
              ("<=",           numBoolBinop (<=)),
              ("&&",           boolBoolBinop (&&)),
              ("||",           boolBoolBinop (||)),
              ("string=?",     strBoolBinop (==)),
              ("string<?",     strBoolBinop (<)),
              ("string>?",     strBoolBinop (>)),
              ("string<=?",    strBoolBinop (<=)),
              ("string>=?",    strBoolBinop (>=)),
              ("car",          car),
              ("cdr",          cdr),
              ("cons",         cons),
              ("eqv",          eqv)
             ]

eqv :: [LispVal] -> ThrowsError LispVal

-- if same type -> cmp values using '=='
eqv [Bool x, Bool y]                    = return $ Bool $ x == y
eqv [Atom x, Atom y]                    = return $ Bool $ x == y
eqv [Number x, Number y]                = return $ Bool $ x == y
eqv [Float x, Float y]                  = return $ Bool $ x == y
eqv [Character x, Character y]          = return $ Bool $ x == y
eqv [String x, String y]                = return $ Bool $ x == y

-- dottedList comparision can be done using list comparision
eqv [DottedList a b, DottedList c d]    = eqv [List (a ++ [b]), List (c ++ [d])]

-- recursively comparing list
eqv [List [], List []]                  = return $ Bool $ True
eqv [List x, List []]                   = return $ Bool $ False
eqv [List [], List y]                   = return $ Bool $ False
eqv [List (x:xs), List (y:ys)]          = do
                                          val <- eqv [List xs, List ys]
                                          case val of
                                            Bool cmp_result -> return $ Bool $ cmp_result && (x == y)

eqv [_, _]                              = return $ Bool $ False
eqv badArgList                          = throwError $ NumArgs 2 badArgList


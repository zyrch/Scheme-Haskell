module Scheme.Evaluator where

import Scheme.Parser.LispVal
import Scheme.LispError
import Control.Monad.Except

import Scheme.Primitives.Binops
import Scheme.Primitives.List
import Scheme.Primitives.List

import Scheme.State

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _)                                       = return val
eval env val@(Number _)                                       = return val
eval env val@(Bool _)                                         = return val
eval env (Atom id)                                            = getVar env id
eval env (List [Atom "quote", val])                           = return val

-- TODO: Add cond and case expressions

eval env (List [Atom "if", pred, conseq, alt])                = 
     do result <- evalPred env pred
        case result of
             False -> eval env alt
             True  -> eval env conseq


eval env (List [Atom "define", Atom varName, varExpr])       = eval env varExpr >>= defineVar env varName
eval env (List [Atom "set!", Atom varName, varExpr])         = eval env varExpr >>= setVar env varName

eval env (List (Atom func : args))                            = (mapM (eval env) args) >>= liftThrowsError . apply func

eval env badForm                                              = throwError $ BadSpecialForm "Unrecognized special form" badForm

evalPred :: Env -> LispVal -> IOThrowsError Bool
evalPred env pred =
     do result <- eval env pred
        case result of
            Bool False -> return $ False
            Bool True  -> return $ True
            otherwise  -> throwError $ TypeMismatch "predicate should be Bool" result

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


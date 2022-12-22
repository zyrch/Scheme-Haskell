module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Control.Monad.Except
import System.Environment
import Data.Char(digitToInt, ord, intToDigit, toUpper)
import System.IO

import Scheme.Parser.ParseExpr
import Scheme.Parser.LispVal
import Scheme.Evaluator
import Scheme.LispError

import Scheme.State

main :: IO ()
main = do 
          args <- getArgs
          case length args of
              0 -> runRepl

runRepl :: IO ()
runRepl = nullEnv >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> until_ pred prompt action

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows (liftM show $ (liftThrowsError $ readExpr expr) >>= eval env) 

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

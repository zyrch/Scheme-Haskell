module Scheme.State where 

import Data.IORef
import Data.List

import Control.Monad.Except
import Scheme.LispError
import Scheme.Parser.LispVal

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

type IOThrowsError = ExceptT LispError IO

-- here 'throwError' and 'return' have definations in the context of IOThrowsError (i.e. transformed monad)
liftThrowsError :: ThrowsError a -> IOThrowsError a
liftThrowsError (Left val)  = throwError val
liftThrowsError (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= (return . extractValue)

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . (lookup var)

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
                      varTable <- liftIO $ readIORef envRef
                      maybe (throwError $ UnboundVar "Getting an unbound variable" var) (liftIO . readIORef) (lookup var varTable)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef varName varVal  = do
                      varTable <- liftIO $ readIORef envRef
                      maybe (throwError $ UnboundVar "Setting an unbound variable" varName)
                            (liftIO . (flip writeIORef varVal))
                            (lookup varName varTable)
                      return varVal

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef varName varVal = do
                                  varTable <- liftIO $ readIORef envRef
                                  maybe (liftIO $ (newIORef varVal >>= addNewVar envRef varName >> return varVal)) 
                                        (const (setVar envRef varName varVal >> return varVal))
                                        (lookup varName varTable)
                                  return varVal

addNewVar :: Env -> String -> IORef LispVal -> IO ()
addNewVar envRef varName varRef = do
                                  varTable <- liftIO $ readIORef envRef
                                  writeIORef envRef ((varName, varRef) : varTable)


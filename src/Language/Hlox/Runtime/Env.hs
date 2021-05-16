{-# LANGUAGE OverloadedStrings #-}

module Language.Hlox.Runtime.Env where

import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Functor
import Data.IORef
import Data.Maybe (isJust)
import Data.Text (Text)
import Language.Hlox.Runtime.Error
import Language.Hlox.Syntax (Value)

type Env = IORef [(Text, IORef Value)]

nullEnv :: IO Env
nullEnv = newIORef []

isBound :: Env -> Text -> IO Bool
isBound envRef var = readIORef envRef <&> (isJust . lookup var)

getVar :: Env -> Text -> IOThrowsError Value
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Getting undefined variable" var) (liftIO . readIORef) (lookup var env)

setVar :: Env -> Text -> Value -> IOThrowsError Value
setVar envRef name value = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Getting undefined variable" name) (liftIO . flip writeIORef value) (lookup name env)
  return value

defineVar :: Env -> Text -> Value -> IOThrowsError Value
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then setVar envRef var value >> return value
    else liftIO $ do
      valueRef <- newIORef value
      env <- readIORef envRef
      writeIORef envRef ((var, valueRef) : env)
      return value

bindVars :: Env -> [(Text, Value)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where
    extendEnv bindings env = fmap (++ env) (mapM addBinding bindings)
    addBinding (var, value) = do
      ref <- newIORef value
      return (var, ref)

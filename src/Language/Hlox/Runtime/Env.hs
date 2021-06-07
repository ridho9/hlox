{-# LANGUAGE OverloadedStrings #-}

module Language.Hlox.Runtime.Env where

import Control.Monad.Except
import Data.Functor
import Data.IORef
import Data.Maybe (isJust)
import Data.Text (Text)
import Language.Hlox.Annotation
import Language.Hlox.Runtime.Error

type Env a = IORef [(Text, IORef a)]

nullEnv :: IO (Env a)
nullEnv = newIORef []

isBound :: Env a -> Text -> IO Bool
isBound envRef var = readIORef envRef <&> (isJust . lookup var)

getVar :: Annotation -> Env a -> Text -> IOThrowsError a
getVar l envRef var = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar l "Getting undefined variable" var) (liftIO . readIORef) (lookup var env)

setVar :: Annotation -> Env a -> Text -> a -> IOThrowsError a
setVar l envRef name value = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ UnboundVar l "Getting undefined variable" name)
    (liftIO . flip writeIORef value)
    (lookup name env)
  return value

defineVar :: Env a -> Text -> a -> IOThrowsError a
defineVar envRef var value = do
  liftIO $ do
    valueRef <- newIORef value
    env <- readIORef envRef
    writeIORef envRef ((var, valueRef) : env)
    return value

bindVars :: Env a -> [(Text, a)] -> IO (Env a)
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where
    extendEnv bindings env = fmap (++ env) (mapM addBinding bindings)
    addBinding (var, value) = do
      ref <- newIORef value
      return (var, ref)

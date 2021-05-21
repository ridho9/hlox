{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Hlox.Runtime.Stmt where

import Control.Monad.IO.Class
import Data.Functor
import Data.Text qualified as T
import Language.Hlox.Runtime.Env
import Language.Hlox.Runtime.Error
import Language.Hlox.Runtime.Expr
import Language.Hlox.Syntax

evalStmts :: Traversable t => Env -> t Statement -> IOThrowsError (t Value)
evalStmts env = mapM (evalStmt env)

evalStmt :: Env -> Statement -> IOThrowsError Value
evalStmt env (Expression expr) = evalExpr env expr
evalStmt env (Print expr) = do
  val <- evalExpr env expr
  liftIO $
    putStrLn $ case val of
      (String s) -> T.unpack s
      _ -> show val
  return Nil
evalStmt env (Declaration name maybeVal) =
  do
    val <- case maybeVal of
      Just expr -> evalExpr env expr
      Nothing -> return Nil
    defineVar env name val
evalStmt env (Block statements) = do
  blockEnv <- liftIO $ bindVars env []
  res <- evalStmts blockEnv statements
  return $ case res of
    [] -> Nil
    l -> last l
evalStmt env (If condition ifTrue ifFalse) = do
  condVal <- evalExpr env condition
  if valueTruty condVal
    then evalStmt env ifTrue
    else case ifFalse of
      Just s -> evalStmt env s
      Nothing -> return Nil

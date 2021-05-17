{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Hlox.Runtime.Stmt where

import Control.Monad.IO.Class
import Data.Functor
import Language.Hlox.Runtime.Env
import Language.Hlox.Runtime.Error
import Language.Hlox.Runtime.Expr
import Language.Hlox.Syntax

evalStmt :: Env -> Statement -> IOThrowsError Value
evalStmt env (Expression expr) = evalExpr env expr
evalStmt env (Print expr) = do
  val <- evalExpr env expr
  (liftIO . print) val
  return Nil
evalStmt env (Declaration name maybeVal) =
  do
    val <- case maybeVal of
      Just expr -> evalExpr env expr
      Nothing -> return Nil
    defineVar env name val

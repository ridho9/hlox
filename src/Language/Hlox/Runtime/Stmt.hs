{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Hlox.Runtime.Stmt where

import Control.Monad.IO.Class
import Data.Functor
import Language.Hlox.Runtime.Env
import Language.Hlox.Runtime.Error
import Language.Hlox.Runtime.Expr
import Language.Hlox.Syntax

evalStmt :: Env -> Statement -> IOThrowsError ()
evalStmt env (Expression expr) = evalExpr env expr $> ()
evalStmt env (Print expr) = evalExpr env expr >>= (liftIO . print)
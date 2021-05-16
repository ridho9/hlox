{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Language.Hlox.Interpreter where

import Control.Monad.Except
import Data.Text
import Data.Text qualified as T
import Language.Hlox.Parser
import Language.Hlox.Runtime.Env
import Language.Hlox.Runtime.Error
import Language.Hlox.Runtime.Expr
import Language.Hlox.Runtime.Stmt
import Language.Hlox.Syntax
import Text.Megaparsec (parse)

parseLine :: Text -> Text -> ThrowsError Statement
parseLine filename input = case parse (sc >> parseStatement) (T.unpack filename) input of
  Left err -> throwError $ Parser err
  Right stmt -> return stmt

interpretLine :: Env -> Text -> Text -> IOThrowsError Text
interpretLine env filename input = case parseLine filename input of
  Left err -> liftIO $ print err >> return ""
  Right stmt -> do
    liftIO
      (runExceptT $ evalStmt env stmt)
      >>= \case
        Left err -> liftIO $ print err
        Right a -> return ()
    return ""
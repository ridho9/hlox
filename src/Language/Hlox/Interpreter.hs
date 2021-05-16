{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
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

interpretLine :: Env -> Text -> Text -> IOThrowsError Text
interpretLine env filename input =
  do
    case parseLine filename input of
      Left err -> do
        liftIO $ putStrLn . T.unpack <$> extractValue $ trapError $ throwError $ Parser err
        return ""
      Right stmt -> do
        evalStmt env stmt
        return ""
  where
    parseLine filename input = parse (sc >> parseStatement) (T.unpack filename) input
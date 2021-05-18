{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Language.Hlox.Interpreter where

import Control.Monad.Except
import Data.Functor
import Data.Text
import Data.Text qualified as T
import Language.Hlox.Parser
import Language.Hlox.Runtime.Env
import Language.Hlox.Runtime.Error
import Language.Hlox.Runtime.Expr
import Language.Hlox.Runtime.Stmt
import Language.Hlox.Syntax
import Text.Megaparsec

parseHlox :: Parser a -> Text -> Text -> ThrowsError a
parseHlox parser filename input = case parse (sc >> parser) (T.unpack filename) input of
  Left err -> throwError $ Parser err
  Right stmt -> return stmt

interpretLine :: Env -> Text -> Text -> IOThrowsError Text
interpretLine = interpret parseProgramLine evalStmt

interpretFile :: Env -> Text -> IOThrowsError Text
interpretFile env filename = do
  content <- liftIO (T.pack <$> readFile (T.unpack filename))
  interpret parseProgram evalStmts env filename content

interpret parser evaluator env filename filetext = do
  program <- liftThrows $ parseHlox parser filename filetext
  evaluator env program
  return ""
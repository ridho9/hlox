{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Hlox.Runtime.Error where

import Control.Arrow
import Control.Monad.Except
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Language.Hlox.Annotation
import Text.Megaparsec

type ParserError = ParseErrorBundle Text Void

type ThrowsError = Either Error

type IOThrowsError = ExceptT Error IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError Text -> IO Text
runIOThrows action = runExceptT (trapError action) <&> extractValue

data Error
  = Parser ParserError
  | RuntimeError Annotation RuntimeError
  | LoopBreak Text

instance Show Error where
  show = T.unpack . showError

showError :: Error -> Text
showError (Parser err) = "Parser error: " <> T.pack (errorBundlePretty err)
showError (LoopBreak message) = "Break error: " <> message
showError (RuntimeError l err) = showPos l <> "Runtime error: " <> showRuntimeError err

data RuntimeError
  = TypeMismatch Text Text
  | UnboundVar Text Text

instance Show RuntimeError where
  show = T.unpack . showRuntimeError

showRuntimeError =
  \case
    UnboundVar message varname -> message <> ": " <> varname
    TypeMismatch expected found -> "Invalid type: expected " <> expected <> ", found " <> found

showPos l = "[" <> locString l <> "] "

trapError action = catchError action (show >>> T.pack >>> return)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
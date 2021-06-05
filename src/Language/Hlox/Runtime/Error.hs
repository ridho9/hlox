{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Hlox.Runtime.Error where

import Control.Monad.Except
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text qualified as T
-- import Language.Hlox.Parser (ParserError)

import Data.Void (Void)
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
  | TypeMismatch Text Text
  | UnboundVar Text Text
  | LoopBreak Text

instance Show Error where
  show = T.unpack . showError

showError :: Error -> Text
showError (UnboundVar message varname) = message <> ": " <> varname
showError (TypeMismatch expected found) = "Invalid type: expected " <> expected <> ", found " <> found
showError (Parser err) = "Parser error: " <> T.pack (errorBundlePretty err)
showError (LoopBreak message) = "Break error: " <> message

trapError action = catchError action (return . (T.pack . show))

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
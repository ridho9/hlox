{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Hlox.Runtime.Error where

import Control.Arrow
import Control.Monad.Except
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Language.Hlox.Syntax
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
  | TypeMismatch Annotation Text Text
  | UnboundVar Annotation Text Text
  | LoopBreak Annotation Text

instance Show Error where
  show = T.unpack . showError

showError :: Error -> Text
showError (Parser err) = "Parser error: " <> T.pack (errorBundlePretty err)
showError (UnboundVar l message varname) = showPos l <> message <> ": " <> varname
showError (TypeMismatch l expected found) = showPos l <> "Invalid type: expected " <> expected <> ", found " <> found
showError (LoopBreak l message) = showPos l <> "Break error: " <> message

showPos l = "[" <> locString l <> "] "

trapError action = catchError action (show >>> T.pack >>> return)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
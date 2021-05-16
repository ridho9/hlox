{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Hlox.Runtime.Error where

import Control.Monad.Except
import Data.Text (Text)
import Data.Text qualified as T
import Language.Hlox.Parser (ParserError)
import Language.Hlox.Syntax (Value)
import Text.Megaparsec (errorBundlePretty)

type ThrowsError = Either Error
data Error
  = Parser ParserError
  | TypeMismatch Text Value

instance Show Error where
  show = T.unpack . showError

showError :: Error -> Text
showError (TypeMismatch expected found) = "Invalid type: expected " <> expected <> ", found " <> T.pack (show found)
showError (Parser err) = "Parser error: " <> T.pack (errorBundlePretty err)

trapError action = catchError action (return . (T.pack . show))

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
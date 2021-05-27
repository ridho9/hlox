{-# LANGUAGE OverloadedStrings #-}

module Language.Hlox.Value where

import Data.Text
import qualified Data.Text as T

data Value
  = Number Double
  | String Text
  | Bool Bool
  | Nil
  deriving (Eq)

instance Show Value where show = T.unpack . showValue

showValue :: Value -> Text
showValue (Number v) = T.pack $ show v
showValue (String v) = T.pack $ show v
showValue (Bool True) = "true"
showValue (Bool False) = "false"
showValue Nil = "nil"

valueType :: Value -> Text
valueType (Number _) = "number"
valueType (String _) = "string"
valueType (Bool _) = "boolean"
valueType Nil = "nil"

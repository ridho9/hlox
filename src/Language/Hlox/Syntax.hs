{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Hlox.Syntax where

import Data.Text (Text)
import qualified Data.Text as T

data Statement
  = Expression Expression
  | Print Expression
  | Declaration Text (Maybe Expression)
  deriving (Show)

data Expression
  = Literal Value
  | Grouping Expression
  | Unary UnaryOp Expression
  | Binary Expression BinaryOp Expression
  | Variable Text
  deriving (Show)

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

data UnaryOp = Not | Negate deriving (Show, Eq)

data BinaryOp
  = Divide
  | Multiply
  | Plus
  | Minus
  | Greater
  | GreaterEq
  | Less
  | LessEq
  | Eq
  | NotEq
  deriving (Show, Eq)
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Hlox.Syntax where

import Data.Text (Text)

data Statement
  = Expression Expression
  | Print Expression
  deriving (Show)

data Expression
  = Literal Value
  | Grouping Expression
  | Unary UnaryOp Expression
  | Binary Expression BinaryOp Expression
  deriving (Show)

data Value
  = Number Double
  | String Text
  | Bool Bool
  | Nil
  deriving (Eq)

instance Show Value where show = showValue

showValue :: Value -> String
showValue (Number v) = show v
showValue (String v) = show v
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
module Language.Hlox.Syntax where

import Data.Text (Text)

data Expression
  = Literal Value
  | Grouping Expression
  | Unary UnaryOp Expression
  | Binary Expression BinaryOp Expression
  deriving (Show, Eq)

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

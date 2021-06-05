{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Hlox.Syntax where

import Data.Text (Text)
import Data.Text qualified as T
import Language.Hlox.Value (Value)
import Text.Megaparsec (SourcePos)

data Statement a
  = Expression a (Expression a)
  | Print a (Expression a)
  | Declaration a Text (Maybe (Expression a))
  | Block a [Statement a]
  | If a (Expression a) (Statement a) (Maybe (Statement a))
  | While a (Expression a) (Statement a)
  | Break a
  deriving (Show)

data Expression a
  = Literal a Value
  | Grouping a (Expression a)
  | Unary a UnaryOp (Expression a)
  | Binary a BinaryOp (Expression a) (Expression a)
  | Variable a Text
  | Assignment a Text (Expression a)
  | Logical a LogicalOp (Expression a) (Expression a)
  | Call a (Expression a) [Expression a]
  deriving (Show)

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

data LogicalOp
  = And
  | Or
  deriving (Show, Eq)
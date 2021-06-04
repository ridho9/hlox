{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Hlox.Syntax where

import Data.Text (Text)
import Data.Text qualified as T
import Language.Hlox.Value (Value)

data Statement
  = Expression Expression
  | Print Expression
  | Declaration Text (Maybe Expression)
  | Block [Statement]
  | If Expression Statement (Maybe Statement)
  | While Expression Statement
  | Break
  deriving (Show)

data Expression
  = Literal Value
  | Grouping Expression
  | Unary UnaryOp Expression
  | Binary BinaryOp Expression Expression
  | Variable Text
  | Assignment Text Expression
  | Logical LogicalOp Expression Expression
  | Call Expression [Expression]
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
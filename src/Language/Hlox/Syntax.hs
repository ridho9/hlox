{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Language.Hlox.Syntax where

import Data.Text (Text)
import Data.Text qualified as T
import Language.Hlox.Value (Value)
import Text.Megaparsec (SourcePos (SourcePos), sourcePosPretty)

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

newtype Annotation = Annotation {loc :: SourcePos}

locString :: Annotation -> Text
locString = T.pack . sourcePosPretty . loc

class Annotated ast where
  ann :: ast a -> a
  amap :: (a -> a) -> ast a -> ast a

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

instance Annotated Statement where
  ann s = case s of
    Expression l _ -> l
    Print l _ -> l
    Declaration l _ _ -> l
    Block l _ -> l
    If l _ _ _ -> l
    While l _ _ -> l
    Break l -> l
  amap f s = case s of
    Expression l a1 -> Expression (f l) a1
    Print l a1 -> Print (f l) a1
    Declaration l a1 a2 -> Declaration (f l) a1 a2
    Block l a1 -> Block (f l) a1
    If l a1 a2 a3 -> If (f l) a1 a2 a3
    While l a1 a2 -> While (f l) a1 a2
    Break l -> Break $ f l

instance Annotated Expression where
  ann e = case e of
    Literal a _ -> a
    Grouping a _ -> a
    Unary a _ _ -> a
    Binary a _ _ _ -> a
    Variable a _ -> a
    Assignment a _ _ -> a
    Logical a _ _ _ -> a
    Call a _ _ -> a
  amap f e = case e of
    Literal l a1 -> Literal (f l) a1
    Grouping l a1 -> Grouping (f l) a1
    Unary l a1 a2 -> Unary (f l) a1 a2
    Binary l a1 a2 a3 -> Binary (f l) a1 a2 a3
    Variable l a1 -> Variable (f l) a1
    Assignment l a1 a2 -> Assignment (f l) a1 a2
    Logical l a1 a2 a3 -> Logical (f l) a1 a2 a3
    Call l a1 a2 -> Call (f l) a1 a2
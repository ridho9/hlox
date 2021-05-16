{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Hlox.Runtime.Expr where

import Control.Monad.Except
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text qualified as T
import Language.Hlox.Parser
import Language.Hlox.Runtime.Error
import Language.Hlox.Syntax
import Text.Megaparsec

evalExpr :: Expression -> ThrowsError Value
evalExpr (Literal v) = return v
evalExpr (Grouping v) = evalExpr v
evalExpr (Unary Not e) = evalExpr e >>= unaryNot
evalExpr (Unary Negate e) = evalExpr e >>= unaryNegate
evalExpr (Binary leftE op rightE) = do
  leftV <- evalExpr leftE
  rightV <- evalExpr rightE
  case lookup op binaryOpList of
    Just opFunc -> opFunc leftV rightV

binaryOpList =
  [ (Plus, binaryPlus)
  , (Minus, binaryMinus)
  , (Multiply, binaryMultiply)
  , (Divide, binaryDivide)
  , (Greater, binaryGreater)
  , (GreaterEq, binaryGreaterEq)
  , (Less, binaryLess)
  , (LessEq, binaryLessEq)
  , (Eq, binaryEq)
  , (NotEq, binaryNotEq)
  ]

binaryNotEq :: Value -> Value -> ThrowsError Value
binaryNotEq (Number left) right = binarySameTypeOp Bool unpackNum (/=) (Number left) right
binaryNotEq (String left) right = binarySameTypeOp Bool unpackStr (/=) (String left) right
binaryNotEq left _ = throwError $ TypeMismatch "number or string" left

binaryEq :: Value -> Value -> ThrowsError Value
binaryEq (Number left) right = binarySameTypeOp Bool unpackNum (==) (Number left) right
binaryEq (String left) right = binarySameTypeOp Bool unpackStr (==) (String left) right
binaryEq left _ = throwError $ TypeMismatch "number or string" left

binaryLessEq :: Value -> Value -> ThrowsError Value
binaryLessEq (Number left) right = binarySameTypeOp Bool unpackNum (<=) (Number left) right
binaryLessEq (String left) right = binarySameTypeOp Bool unpackStr (<=) (String left) right
binaryLessEq left _ = throwError $ TypeMismatch "number or string" left

binaryLess :: Value -> Value -> ThrowsError Value
binaryLess (Number left) right = binarySameTypeOp Bool unpackNum (<) (Number left) right
binaryLess (String left) right = binarySameTypeOp Bool unpackStr (<) (String left) right
binaryLess left _ = throwError $ TypeMismatch "number or string" left

binaryGreaterEq :: Value -> Value -> ThrowsError Value
binaryGreaterEq (Number left) right = binarySameTypeOp Bool unpackNum (>=) (Number left) right
binaryGreaterEq (String left) right = binarySameTypeOp Bool unpackStr (>=) (String left) right
binaryGreaterEq left _ = throwError $ TypeMismatch "number or string" left

binaryGreater :: Value -> Value -> ThrowsError Value
binaryGreater (Number left) right = binarySameTypeOp Bool unpackNum (>) (Number left) right
binaryGreater (String left) right = binarySameTypeOp Bool unpackStr (>) (String left) right
binaryGreater left _ = throwError $ TypeMismatch "number or string" left

binaryMultiply :: Value -> Value -> ThrowsError Value
binaryMultiply = binaryNumberOp (*)

binaryDivide :: Value -> Value -> ThrowsError Value
binaryDivide = binaryNumberOp (/)

binaryMinus :: Value -> Value -> ThrowsError Value
binaryMinus = binaryNumberOp (-)

binaryPlus :: Value -> Value -> ThrowsError Value
binaryPlus (Number left) right = binaryNumberOp (+) (Number left) right
binaryPlus (String left) right = binaryStringOp (<>) (String left) right
binaryPlus left _ = throwError $ TypeMismatch "number or string" left

binaryNumberOp = binarySameTypeOp Number unpackNum

binaryStringOp = binarySameTypeOp String unpackStr

binarySameTypeOp :: (b -> Value) -> Unpacker a -> (a -> a -> b) -> Value -> Value -> ThrowsError Value
binarySameTypeOp resContainer unpacker op left right = do
  l <- unpacker left
  r <- unpacker right
  return $ resContainer $ op l r

unaryNegate :: Value -> ThrowsError Value
unaryNegate v = unpackNum v <&> (Number . negate)

unaryNot :: Value -> ThrowsError Value
unaryNot v = case valueTruty v of Bool v -> return $ Bool $ not v

valueTruty :: Value -> Value
valueTruty Nil = Bool False
valueTruty (Bool v) = Bool v
valueTruty _ = Bool True

type Unpacker a = Value -> ThrowsError a

unpackNum :: Unpacker Double
unpackNum (Number n) = return n
unpackNum v = throwError $ TypeMismatch "number" v

unpackStr :: Unpacker Text
unpackStr (String n) = return n
unpackStr v = throwError $ TypeMismatch "string" v
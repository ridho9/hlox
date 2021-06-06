{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Hlox.Runtime.Expr where

import Control.Monad.Except
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text qualified as T
import Language.Hlox.Parser
import Language.Hlox.Runtime.Env
import Language.Hlox.Runtime.Error
import Language.Hlox.Syntax
import Language.Hlox.Value
import Text.Megaparsec

evalExpr :: Env Value -> Expression Annotation -> IOThrowsError Value
evalExpr env (Literal _ v) = return v
evalExpr env (Grouping _ v) = evalExpr env v
evalExpr env (Unary _ Not e) = evalExpr env e >>= (liftThrows . unaryNot)
evalExpr env (Unary _ Negate e) = evalExpr env e >>= (liftThrows . unaryNegate)
evalExpr env (Variable _ var) = getVar env var
evalExpr env (Binary _ op leftE rightE) = do
  leftV <- evalExpr env leftE
  rightV <- evalExpr env rightE
  case lookup op binaryOpList of
    Just opFunc -> liftThrows $ opFunc leftV rightV
evalExpr env (Assignment _ name expr) = do
  val <- evalExpr env expr
  setVar env name val
evalExpr env (Logical _ And leftE rightE) = do
  leftV <- evalExpr env leftE
  if valueTruthy leftV
    then evalExpr env rightE
    else return leftV
evalExpr env (Logical _ Or leftE rightE) = do
  leftV <- evalExpr env leftE
  if valueTruthy leftV
    then return leftV
    else evalExpr env rightE

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

type BinaryOpFunc = Value -> Value -> ThrowsError Value

binaryNotEq :: BinaryOpFunc
binaryNotEq left right = return $ Bool $ left /= right

binaryEq :: BinaryOpFunc
binaryEq left right = return $ Bool $ left == right

binaryLessEq :: BinaryOpFunc
binaryLessEq (Number left) right = binarySameTypeOp Bool unpackNum (<=) (Number left) right
binaryLessEq (String left) right = binarySameTypeOp Bool unpackStr (<=) (String left) right
binaryLessEq left _ = throwError $ TypeMismatch undefined "number or string" (showValue left)

binaryLess :: BinaryOpFunc
binaryLess (Number left) right = binarySameTypeOp Bool unpackNum (<) (Number left) right
binaryLess (String left) right = binarySameTypeOp Bool unpackStr (<) (String left) right
binaryLess left _ = throwError $ TypeMismatch undefined "number or string" (showValue left)

binaryGreaterEq :: BinaryOpFunc
binaryGreaterEq (Number left) right = binarySameTypeOp Bool unpackNum (>=) (Number left) right
binaryGreaterEq (String left) right = binarySameTypeOp Bool unpackStr (>=) (String left) right
binaryGreaterEq left _ = throwError $ TypeMismatch undefined "number or string" (showValue left)

binaryGreater :: BinaryOpFunc
binaryGreater (Number left) right = binarySameTypeOp Bool unpackNum (>) (Number left) right
binaryGreater (String left) right = binarySameTypeOp Bool unpackStr (>) (String left) right
binaryGreater left _ = throwError $ TypeMismatch undefined "number or string" (showValue left)

binaryMultiply :: BinaryOpFunc
binaryMultiply = binaryNumberOp (*)

binaryDivide :: BinaryOpFunc
binaryDivide = binaryNumberOp (/)

binaryMinus :: BinaryOpFunc
binaryMinus = binaryNumberOp (-)

binaryPlus :: BinaryOpFunc
binaryPlus (Number left) right = binaryNumberOp (+) (Number left) right
binaryPlus (String left) right = binaryStringOp (<>) (String left) right
binaryPlus left _ = throwError $ TypeMismatch undefined "number or string" (showValue left)

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
unaryNot v = case valueTruthy v of v -> return $ Bool $ not v

valueTruthy :: Value -> Bool
valueTruthy Nil = False
valueTruthy (Bool v) = v
valueTruthy _ = True

type Unpacker a = Value -> ThrowsError a

unpackNum :: Unpacker Double
unpackNum (Number n) = return n
unpackNum v = throwError $ TypeMismatch undefined "number" (showValue v)

unpackStr :: Unpacker Text
unpackStr (String n) = return n
unpackStr v = throwError $ TypeMismatch undefined "string" (showValue v)
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Hlox.Runtime.Expr where

import Control.Monad.Except
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text qualified as T
import Language.Hlox.Annotation
import Language.Hlox.Runtime.Env
import Language.Hlox.Runtime.Error
import Language.Hlox.Syntax
import Language.Hlox.Value

evalExpr :: Env Value -> Expression Annotation -> IOThrowsError Value
evalExpr env (Literal _ v) = return v
evalExpr env (Grouping _ v) = evalExpr env v
evalExpr env (Unary l Not e) = evalExpr env e >>= (liftThrows . unaryNot l)
evalExpr env (Unary l Negate e) = evalExpr env e >>= (liftThrows . unaryNegate l)
evalExpr env (Variable l var) = getVar l env var
evalExpr env (Binary l op leftE rightE) = do
  leftV <- evalExpr env leftE
  rightV <- evalExpr env rightE
  case lookup op (binaryOpList l) of
    Just opFunc -> liftThrows $ opFunc leftV rightV
evalExpr env (Assignment l name expr) = do
  val <- evalExpr env expr
  setVar l env name val
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

binaryOpList loc =
  [ (Plus, binaryPlus loc)
  , (Minus, binaryMinus loc)
  , (Multiply, binaryMultiply loc)
  , (Divide, binaryDivide loc)
  , (Greater, binaryGreater loc)
  , (GreaterEq, binaryGreaterEq loc)
  , (Less, binaryLess loc)
  , (LessEq, binaryLessEq loc)
  , (Eq, binaryEq loc)
  , (NotEq, binaryNotEq loc)
  ]

type BinaryOpFunc = Annotation -> Value -> Value -> ThrowsError Value

binaryNotEq :: BinaryOpFunc
binaryNotEq _ left right = return $ Bool $ left /= right

binaryEq :: BinaryOpFunc
binaryEq _ left right = return $ Bool $ left == right

binaryLessEq :: BinaryOpFunc
binaryLessEq l (Number left) right = binarySameTypeOp Bool unpackNum l (<=) (Number left) right
binaryLessEq l (String left) right = binarySameTypeOp Bool unpackStr l (<=) (String left) right
binaryLessEq l left _ = throwError $ RuntimeError l $ TypeMismatch "number or string" (showValue left)

binaryLess :: BinaryOpFunc
binaryLess l (Number left) right = binarySameTypeOp Bool unpackNum l (<) (Number left) right
binaryLess l (String left) right = binarySameTypeOp Bool unpackStr l (<) (String left) right
binaryLess l left _ = throwError $ RuntimeError l $ TypeMismatch "number or string" (showValue left)

binaryGreaterEq :: BinaryOpFunc
binaryGreaterEq l (Number left) right = binarySameTypeOp Bool unpackNum l (>=) (Number left) right
binaryGreaterEq l (String left) right = binarySameTypeOp Bool unpackStr l (>=) (String left) right
binaryGreaterEq l left _ = throwError $ RuntimeError l $ TypeMismatch "number or string" (showValue left)

binaryGreater :: BinaryOpFunc
binaryGreater l (Number left) right = binarySameTypeOp Bool unpackNum l (>) (Number left) right
binaryGreater l (String left) right = binarySameTypeOp Bool unpackStr l (>) (String left) right
binaryGreater l left _ = throwError $ RuntimeError l $ TypeMismatch "number or string" (showValue left)

binaryMultiply :: BinaryOpFunc
binaryMultiply l = binaryNumberOp l (*)

binaryDivide :: BinaryOpFunc
binaryDivide l = binaryNumberOp l (/)

binaryMinus :: BinaryOpFunc
binaryMinus l = binaryNumberOp l (-)

binaryPlus :: BinaryOpFunc
binaryPlus l (Number left) right = binaryNumberOp l (+) (Number left) right
binaryPlus l (String left) right = binaryStringOp l (<>) (String left) right
binaryPlus l left _ = throwError $ RuntimeError l $ TypeMismatch "number or string" (showValue left)

binaryNumberOp = binarySameTypeOp Number unpackNum

binaryStringOp = binarySameTypeOp String unpackStr

binarySameTypeOp :: (b -> Value) -> Unpacker a -> Annotation -> (a -> a -> b) -> Value -> Value -> ThrowsError Value
binarySameTypeOp resContainer unpacker ann op left right = do
  l <- unpacker ann left
  r <- unpacker ann right
  return $ resContainer $ op l r

type UnaryOpFunc = Annotation -> Value -> ThrowsError Value

unaryNegate :: UnaryOpFunc
unaryNegate l v = unpackNum l v <&> (Number . negate)

unaryNot :: UnaryOpFunc
unaryNot l v = case valueTruthy v of v -> return $ Bool $ not v

valueTruthy :: Value -> Bool
valueTruthy Nil = False
valueTruthy (Bool v) = v
valueTruthy _ = True

type Unpacker a = Annotation -> Value -> ThrowsError a

unpackNum :: Unpacker Double
unpackNum _ (Number n) = return n
unpackNum l v = throwError $ RuntimeError l $ TypeMismatch "number" (showValue v)

unpackStr :: Unpacker Text
unpackStr _ (String n) = return n
unpackStr l v = throwError $ RuntimeError l $ TypeMismatch "string" (showValue v)
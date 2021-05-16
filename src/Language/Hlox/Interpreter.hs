{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Language.Hlox.Interpreter where

import Control.Monad.Except
import Data.Functor
import Data.Text (Text)
import Data.Text qualified as T
import Language.Hlox.Parser
import Language.Hlox.Syntax
import Text.Megaparsec

data Error
  = Parser ParserError
  | TypeMismatch Text Value

instance Show Error where
  show = T.unpack . showError

showError :: Error -> Text
showError (TypeMismatch expected found) = "Invalid type: expected " <> expected <> ", found " <> T.pack (show found)
showError (Parser err) = "Parser error: " <> T.pack (errorBundlePretty err)

type ThrowsError = Either Error

interpretLine :: String -> String -> ThrowsError Value
interpretLine filename input =
  let parseLine filename input = parse (sc >> parseExpression) filename (T.pack input)
   in do
        case parseLine filename input of
          Left err -> throwError $ Parser err
          Right val -> eval val

eval :: Expression -> ThrowsError Value
eval (Literal v) = return v
eval (Grouping v) = eval v
eval (Unary Not e) = eval e >>= unaryNot
eval (Unary Negate e) = eval e >>= unaryNegate
eval (Binary leftE op rightE) = do
  leftV <- eval leftE
  rightV <- eval rightE
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

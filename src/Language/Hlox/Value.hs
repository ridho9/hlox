{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Hlox.Value where

import Control.Monad (when)
import Control.Monad.Error.Class
import Data.Text (Text)
import Data.Text qualified as T
import Language.Hlox.Annotation
import Language.Hlox.Runtime.Env
import Language.Hlox.Runtime.Error

data Value
  = Number Double
  | String Text
  | Bool Bool
  | Nil
  | Callable Callable
  deriving (Eq)

call :: Annotation -> Env Value -> Value -> [Value] -> IOThrowsError Value
call l env callee args =
  do
    case callee of
      Callable f -> callCallable l env f args
      _ -> throwError $ RuntimeError l $ TypeMismatch "function" (showValue callee)

instance Show Value where show = T.unpack . showValue

showValue :: Value -> Text
showValue (Number v) = T.pack $ show v
showValue (String v) = T.pack $ show v
showValue (Bool True) = "true"
showValue (Bool False) = "false"
showValue Nil = "nil"
showValue (Callable c) = showCallable c

data Callable
  = NativeFunc (Maybe Int) (Env Value -> [Value] -> IOThrowsError Value)

callCallable :: Annotation -> Env Value -> Callable -> [Value] -> IOThrowsError Value
callCallable l env c args =
  do
    checkArity c args
    case c of
      NativeFunc _ f -> f env args
  where
    checkArity :: Callable -> [a] -> IOThrowsError ()
    checkArity callee args = do
      case arity callee of
        Just expect ->
          when (length args /= expect) $ throwError $ RuntimeError l $ ArityMismatch expect (length args)
        Nothing -> return ()

arity :: Callable -> Maybe Int
arity = \case
  NativeFunc arr _ -> arr

instance Eq Callable where
  _ == _ = False

instance Show Callable where show = T.unpack . showCallable

showCallable c = case c of
  NativeFunc _ _ -> "<native>/" <> arr
  where
    arr = case arity c of
      Just n -> T.pack $ show n
      Nothing -> "-"

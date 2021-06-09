{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad.Except (MonadIO (liftIO), runExceptT)
import Data.Text qualified as T
import Data.Time.Clock.POSIX
import Language.Hlox.Interpreter
import Language.Hlox.Runtime.Env
import Language.Hlox.Runtime.Error
import Language.Hlox.Syntax
import Language.Hlox.Value (Callable (NativeFunc), Value (Callable, Nil, Number, String))
import System.Environment
import System.IO (hFlush, stdout)

tShow = T.pack . show

defineGlobals :: Env Value -> IOThrowsError Value
defineGlobals env = do
  defineVar
    env
    "clock"
    ( Callable $
        NativeFunc
          (Just 0)
          ( \env args -> do
              time <- liftIO getPOSIXTime
              return $ Number (realToFrac time)
          )
    )

main :: IO ()
main = do
  args <- getArgs
  env <- nullEnv

  -- definingGlobals
  runExceptT $ defineGlobals env

  res <- runExceptT $ case args of
    [] -> runRepl env
    filename : _ -> runFile filename env
  case res of
    Left err -> print err
    _ -> return ()

runFile :: String -> Env Value -> IOThrowsError ()
runFile filename env = do
  res <- interpretFile env (T.pack filename)
  return ()

runRepl :: Env Value -> IOThrowsError ()
runRepl env =
  liftIO $ runReplLine env 1
  where
    runReplLine :: Env Value -> Integer -> IO ()
    runReplLine env n =
      do
        res <- runExceptT f
        case res of
          Left err -> print err >> runReplLine env n
          _ -> runReplLine env (n + 1)
      where
        f = do
          expr <- liftIO $ putStr (show n <> "> ") >> hFlush stdout >> getLine
          interpretLine env ("repl-" <> tShow n) (T.pack expr)
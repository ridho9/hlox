{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Except (runExceptT)
import Data.Text qualified as T
import Language.Hlox.Interpreter
import Language.Hlox.Runtime.Env
import Language.Hlox.Runtime.Error
import Language.Hlox.Syntax
import System.Environment
import System.IO (hFlush, stdout)

tShow = T.pack . show

main :: IO ()
main = do
  args <- getArgs
  env <- nullEnv
  case args of
    [] -> runRepl env
    filename : _ -> runFile filename env

runFile :: String -> Env -> IO ()
runFile filename env = do
  res <- runExceptT $ interpretFile env (T.pack filename)
  case res of
    Left err -> print err
    Right _ -> return ()

runRepl :: Env -> IO ()
runRepl env =
  do
    -- runExceptT (defineVar env "name" (String "ridho")) >>= \case
    --   Left err -> print err
    --   Right _ -> return ()
    runReplLine env 1
  where
    runReplLine :: Env -> Integer -> IO ()
    runReplLine env n = do
      expr <- putStr (show n <> "> ") >> hFlush stdout >> getLine
      runIOThrows (interpretLine env ("repl-" <> tShow n) (T.pack expr))
      runReplLine env (n + 1)
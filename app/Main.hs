{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Except (MonadIO (liftIO), runExceptT)
import Data.Text qualified as T
import Language.Hlox.Interpreter
import Language.Hlox.Runtime.Env
import Language.Hlox.Runtime.Error
import Language.Hlox.Syntax
import Language.Hlox.Value (Value (String))
import System.Environment
import System.IO (hFlush, stdout)

tShow = T.pack . show

main :: IO ()
main = do
  args <- getArgs
  env <- nullEnv

  -- definingGlobals
  -- _ <- runExceptT $ defineVar env "name" (String "ridho")

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
  do
    runReplLine env 1
  where
    runReplLine :: Env Value -> Integer -> IOThrowsError ()
    runReplLine env n = do
      expr <- liftIO $ putStr (show n <> "> ") >> hFlush stdout >> getLine
      interpretLine env ("repl-" <> tShow n) (T.pack expr)
      runReplLine env (n + 1)
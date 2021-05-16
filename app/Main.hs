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
import System.IO (hFlush, stdout)

tShow = T.pack . show

runRepl :: IO ()
runRepl =
  do
    env <- nullEnv
    -- runExceptT (defineVar env "name" (String "ridho")) >>= \case
    --   Left err -> print err
    --   Right _ -> return ()
    runReplLine env 1
  where
    runReplLine :: Env -> Integer -> IO ()
    runReplLine env n = do
      expr <- putStr (show n <> "> ") >> hFlush stdout >> getLine
      fmap (putStrLn . T.unpack) $ runIOThrows $ interpretLine env ("repl-" <> tShow n) (T.pack expr)
      runReplLine env (n + 1)

main :: IO ()
main = do
  runRepl

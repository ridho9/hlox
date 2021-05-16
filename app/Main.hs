{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class
import Data.Text
import Data.Text qualified as T
import Language.Hlox.Interpreter
import Language.Hlox.Runtime.Env
import Language.Hlox.Runtime.Error
import System.IO (hFlush, stdout)

tShow = T.pack . show

runRepl :: IO ()
runRepl =
  runReplLine 1
  where
    runReplLine :: Integer -> IO ()
    runReplLine n = do
      expr <- putStr (show n <> "> ") >> hFlush stdout >> getLine
      env <- liftIO nullEnv
      fmap (putStrLn . T.unpack) $ runIOThrows $ interpretLine env ("repl-" <> tShow n) (T.pack expr)
      runReplLine (n + 1)

main :: IO ()
main = do
  runRepl
  main

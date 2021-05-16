{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text
import Data.Text qualified as T
import Language.Hlox.Interpreter
import Language.Hlox.Runtime.Error
import System.IO (hFlush, stdout)

processLine = do
  expr <- putStr "> " >> hFlush stdout >> getLine
  putStrLn $ T.unpack $ extractValue $ trapError $ interpretLine "repl" (T.pack expr)

main :: IO ()
main = do
  processLine
  main

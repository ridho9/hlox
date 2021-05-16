module Main where

import Language.Hlox.Interpreter
import System.IO (hFlush, stdout)

processLine = do
  expr <- putStr "> " >> hFlush stdout >> getLine
  putStrLn $ case interpretLine "repl" expr of
    Left err -> "Error: " <> show err
    Right val -> show val

main :: IO ()
main = do
  processLine
  main

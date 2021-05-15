module Main where

import Language.Hlox.Parser
import System.IO (hFlush, stdout)

processLine = do
  expr <- putStr "> " >> hFlush stdout >> getLine
  putStrLn $ readExpr expr

main :: IO ()
main = do
  processLine
  main

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Language.Hlox.Interpreter where

import Control.Monad.Except
import Data.Text
import Data.Text qualified as T
import Language.Hlox.Parser
import Language.Hlox.Runtime.Error
import Language.Hlox.Runtime.Expr
import Language.Hlox.Syntax
import Text.Megaparsec (parse)

-- interpretLine :: String -> String -> ThrowsError Value
interpretLine :: String -> String -> ThrowsError Text
interpretLine filename input =
  let parseLine filename input = parse (sc >> parseStatement) filename (T.pack input)
   in do
        case parseLine filename input of
          Left err -> throwError $ Parser err
          Right val -> return $ (T.pack . show) val
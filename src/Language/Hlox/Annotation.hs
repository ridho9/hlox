module Language.Hlox.Annotation where

import Data.Text
import Data.Text qualified as T
import Text.Megaparsec

newtype Annotation = Annotation {loc :: SourcePos}

locString :: Annotation -> Text
locString = T.pack . sourcePosPretty . loc

class Annotated ast where
  ann :: ast a -> a
  amap :: (a -> a) -> ast a -> ast a
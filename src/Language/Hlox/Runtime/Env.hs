module Language.Hlox.Runtime.Env where

import Data.IORef (IORef, newIORef)
import Language.Hlox.Syntax (Value)

type Env = IORef [(String, IORef Value)]

nullEnv :: IO Env
nullEnv = newIORef []
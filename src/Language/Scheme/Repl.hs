module Language.Scheme.Repl where

import           Language.Scheme.Internal.Core   (defaultEnv)
import Language.Scheme.Internal.Eval

import qualified Data.Text as T

repl = doExpr defaultEnv . T.pack

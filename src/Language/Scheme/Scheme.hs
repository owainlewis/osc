module Language.Scheme.Scheme where

import           Language.Scheme.Internal.AST
import           Language.Scheme.Internal.Eval
import           Language.Scheme.Internal.Parser

import qualified Data.Text.IO as TIO

runScheme = do
  contents <- TIO.readFile "test/scheme/1.scm"
  return $ readExprsFromFile contents

module Language.Scheme.Scheme
  ( executeFile)
  where

import           Language.Scheme.Internal.AST
import           Language.Scheme.Internal.Core
import           Language.Scheme.Internal.Eval
import           Language.Scheme.Internal.Parser

import qualified Data.Text.IO                    as TIO

-- | Given a file path, read the contents and evaulate it
--
executeFile :: FilePath -> IO Scheme
executeFile filePath = do
  contents <- TIO.readFile filePath
  runSchemeWithDefaultEnv (evalSchemeText contents)

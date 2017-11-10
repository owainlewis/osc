module Language.Scheme.Scheme
    ( runSchemeFile
    ) where

import           Language.Scheme.Internal.AST(Scheme)
import           Language.Scheme.Internal.Eval

import qualified Data.Text.IO as TIO

-- | Given a file path, read the contents and evaulate it
--
runSchemeFile :: FilePath -> IO Scheme
runSchemeFile filePath = do
  contents <- TIO.readFile filePath
  runSchemeWithDefaultEnv (evalSchemeText contents)

module Language.Scheme.Scheme
  ( executeFile
  , executeFileAST
  )

  where

import           Language.Scheme.Internal.AST
import           Language.Scheme.Internal.Core
import           Language.Scheme.Internal.Eval
import           Language.Scheme.Internal.Parser

import qualified Data.Bifunctor as BF
import qualified Data.Text.IO                    as TIO

-- | Useful for debugging the AST returned by parser
--
executeFileAST :: FilePath -> IO (Either String Scheme)
executeFileAST filePath =
    fmap (BF.first show) result
    where result =
            readExprsFromFile <$> TIO.readFile filePath

-- | Given a file path, read the contents and evaulate it
--
executeFile :: FilePath -> IO Scheme
executeFile filePath = do
  contents <- TIO.readFile filePath
  runSchemeWithDefaultEnv (evalSchemeText contents)

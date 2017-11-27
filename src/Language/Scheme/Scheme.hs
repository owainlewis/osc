module Language.Scheme.Scheme
  ( executeFile
  , executeFileAST
  , executeString
  , execute
  )
  where

import           Language.Scheme.Internal.AST
import           Language.Scheme.Internal.Core
import           Language.Scheme.Internal.Eval
import           Language.Scheme.Internal.Parser

import qualified Data.Bifunctor                  as BF

import qualified Data.Text                       as T
import qualified Data.Text.IO                    as TIO

-- | Useful for debugging the AST returned by parser
--
--   When executed, this will parse the contents of a text
--   file and dump the parsed AST
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

execute :: T.Text -> IO Scheme
execute = doExpr defaultEnv

executeString :: String -> IO Scheme
executeString = execute . T.pack

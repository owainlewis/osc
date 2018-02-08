{-# LANGUAGE OverloadedStrings         #-}
module Language.Scheme.Scheme
    ( compileFile
    , compileText
    )
    where

import qualified Data.Text as T

import Control.Monad(mapM_)

import Language.Scheme.Internal.Parser (parseText, parseFile)
import qualified Language.Scheme.Internal.Eval as Eval
import qualified Language.Scheme.Internal.Core as Core
import qualified Language.Scheme.Internal.AST as AST

compileASTs :: AST.Scheme -> IO ()
compileASTs (AST.List x) = mapM_ compileAST x

compileAST :: AST.Scheme -> IO AST.Scheme
compileAST ast = do
    let exprs = Eval.eval ast
    Eval.runScheme exprs Core.defaultEnv

compileFile :: FilePath -> IO ()
compileFile = parseFile `andThen` compileASTs

compileText :: T.Text -> IO ()
compileText = compileASTs . parseText

compileTextAST :: T.Text -> IO AST.Scheme
compileTextAST = compileAST <$> parseText

andThen :: Monad m => (t -> m a) -> (a -> m b) -> t -> m b
andThen f g x = f x >>= g

{-# LANGUAGE OverloadedStrings          #-}
module Language.Scheme.Internal.Core where

import Language.Scheme.Internal.AST

import Control.Monad(foldM)
import Control.Exception(throw)

type Unary  = Scheme -> Eval Scheme
type Binary = Scheme -> Scheme -> Eval Scheme

liftF :: ([Scheme] -> Eval Scheme) -> Scheme
liftF = Fun . IFunc

basicEnv = [("+", plusF)]

plusF :: Scheme
plusF = liftF $ binopFold (numOp (+)) (Number 0)

binopFold :: Binary -> Scheme -> [Scheme] -> Eval Scheme
binopFold op seed args = case args of
                            [a,b]  -> op a b
                            (a:as) -> foldM op seed args
                            []-> throw $ ArityException 0 2

numOp op (Number x) (Number y) = return $ Number $ op x  y
numOp op Nil        (Number y) = return $ Number y
numOp op (Number x) Nil        = return $ Number x
numOp op x           y         = throw $ TypeException "must be numeric"

{-# LANGUAGE OverloadedStrings          #-}
module Language.Scheme.Internal.Core
  ( defaultEnv
  ) where

import Language.Scheme.Internal.AST

import qualified Data.Map as Map

import Control.Monad(foldM)
import Control.Exception(throw)

type Unary  = Scheme -> Eval Scheme
type Binary = Scheme -> Scheme -> Eval Scheme

liftF :: ([Scheme] -> Eval Scheme) -> Scheme
liftF = Fun . IFunc

defaultEnv :: EnvCtx
defaultEnv = Map.fromList
    [ ("+", plusF)
    , ("*", multF)
    , ("car", carF)
    , ("cdr", cdrF)
    , ("cons", consF)
    , ("quote", quoteF)
    ]

-------------------------------------------------------------

plusF :: Scheme
plusF = liftF $ binopFold (numOp (+)) (Number 0)

multF :: Scheme
multF = liftF $ binopFold (numOp (*)) (Number 1)

carF :: Scheme
carF = liftF car

cdrF :: Scheme
cdrF = liftF cdr

quoteF :: Scheme
quoteF = liftF quote

consF :: Scheme
consF = liftF cons

-------------------------------------------------------------

car :: Monad m => [Scheme] -> m Scheme
car [List []] = return Nil
car [List (x:_)] = return x
car [] = return Nil
car _ = throw $ ArgumentException "Expected list"

cdr :: Monad m => [Scheme] -> m Scheme
cdr [List (x:xs)] = return (List xs)
cdr [List []] = return Nil
cdr [] = return Nil
cdr _ = throw $ ArgumentException "Expected list"

cons :: Monad m => [Scheme] -> m Scheme
cons [x,y@(List xs)] = return $ List (x:xs)
cons [x,y] = return $ List [x,y]
cons xs = throw $ ArityException (length xs) 2

quote :: Monad m => [Scheme] -> m Scheme
quote [List xs] = return $ List (Atom "quote" : xs)
quote [expr] = return $ List (Atom "quote" : [expr])
quote [] = throw $ ArityException 0 1

-------------------------------------------------------------

binopFold :: Binary -> Scheme -> [Scheme] -> Eval Scheme
binopFold op seed args = case args of
                            [a,b]  -> op a b
                            (a:as) -> foldM op seed args
                            []-> throw $ ArityException 0 2

-------------------------------------------------------------

numOp op (Number x) (Number y) = return $ Number $ op x  y
numOp op Nil        (Number y) = return $ Number y
numOp op (Number x) Nil        = return $ Number x
numOp op x           y         = throw $ TypeException "must be numeric"

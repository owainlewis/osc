{-# LANGUAGE OverloadedStrings #-}
module Language.Scheme.Internal.Eval
    ( eval
    , runScheme
    , runSchemeWithDefaultEnv
    ) where

import           Language.Scheme.Internal.AST

import           Control.Exception
import           Control.Monad.Reader
import qualified Data.Text                    as T

import qualified Data.Map                     as Map

eval :: Scheme -> Eval Scheme
eval (Number i)                 = return (Number i)
eval (String s)                 = return (String s)
eval (Bool b)                   = return (Bool b)
eval (List [])                  = return Nil
eval Nil                        = return Nil
eval (Atom a)                   = getEnv a
eval (List [Atom "quote", val]) = return val

eval (List [Atom "if", predicate, t, f]) = do
   result <- eval predicate
   case result of
     (Bool True) -> eval t
     (Bool False) -> eval f
     _ -> throw $ GenericException "Expected boolean clause in if"

getEnv :: T.Text -> Eval Scheme
getEnv atom = do
  env <- ask
  case Map.lookup atom env of
    Just x  -> return x
    Nothing -> throw $ UnboundVar atom

setEnv :: T.Text -> Scheme -> Eval T.Text
setEnv name expr = do
    env     <- ask
    evalVal <- eval expr
    local (const $ Map.insert name  evalVal env) (pure name)

--------------------------------------------------------
runScheme :: Eval a -> EnvCtx -> IO a
runScheme expr env = runReaderT (unEval expr) env

runSchemeWithDefaultEnv :: Eval a -> IO a
runSchemeWithDefaultEnv = flip runScheme Map.empty

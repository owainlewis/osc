{-# LANGUAGE OverloadedStrings #-}
module Language.Scheme.Internal.Eval
    ( eval
    , runScheme
    , runSchemeWithDefaultEnv
    , evalSchemeText
    , doExpr
    ) where

import           Language.Scheme.Internal.AST
import           Language.Scheme.Internal.Core   (defaultEnv)

import           Control.Exception
import           Control.Monad.Reader
import qualified Data.Map                        as Map
import qualified Data.Text                       as T
import qualified Language.Scheme.Internal.Parser as P
import Data.Semigroup((<>))

-- Tracer function for debugging
debugEval :: Eval Scheme
debugEval = do
  env <- ask
  liftIO . print $ env
  return Nil

pairMap :: (a -> a) -> (a -> a) -> [a] -> [(a,a)]
pairMap _ _ [] = []
pairMap f g (x:y:xs) = (f x, g y) : pairMap f g xs
pairMap f g (x:xs) = pairMap f g xs

unwindLetForms :: [Scheme] -> Eval [(T.Text, Scheme)]
unwindLetForms [] = return []
unwindLetForms ((Atom x):y:xs) = do
  y' <- eval y
  return [(x, y')]

eval :: Scheme -> Eval Scheme
eval (Number i)                 = return (Number i)
eval (String s)                 = return (String s)
eval (Bool b)                   = return (Bool b)
eval (List [])                  = return Nil
eval (Nil)                      = return Nil
eval (Atom a)                   = getVar a
-- Quoted values
eval (List [Atom "quote", val]) = return val
-- If statements
eval (List [Atom "if", predicate, t, f]) = do
   result <- eval predicate
   case result of
     (Bool True)  -> eval t
     (Bool False) -> eval f
     _            -> throw $ GenericException "Expected boolean clause in if"
-- This state represents simple function application.
-- We first lookup the first argument (f xs) and then apply
-- the evaluated inner fn to the evaluated args

eval (List [Atom "let", (List boundVars), expr]) = do
  env <- ask
  local (const (Map.fromList [] <> env))
  debugEval

eval (List (x:xs)) = do
  env    <- ask
  -- The scheme function to apply
  sf     <- eval x
  -- The arguments to apply a function to
  args   <- mapM eval xs
  case sf of
    (Fun (IFunc f)) -> f args
    _               -> throw $ GenericException "Not a function"
eval _ = throw $ GenericException "Unbound eval form"

--------------------------------------------------------

getVar :: T.Text -> Eval Scheme
getVar atom = do
  env <- ask
  case Map.lookup atom env of
    Just x  -> return x
    Nothing -> throw $ UnboundVar atom

setEnv :: T.Text -> Scheme -> Eval T.Text
setEnv name expr = do
    env     <- ask
    evalVal <- eval expr
    local (const $ Map.insert name evalVal env) (pure name)

--------------------------------------------------------

evalSchemeText :: T.Text -> Eval Scheme
evalSchemeText input = either f g $ P.readExpr input
    where f = throw . GenericException . T.pack . show
          g = eval

runScheme :: Eval a -> EnvCtx -> IO a
runScheme expr env = runReaderT (unEval expr) env

runSchemeWithDefaultEnv :: Eval a -> IO a
runSchemeWithDefaultEnv = flip runScheme defaultEnv

----------------------------------------------------------

doExpr :: EnvCtx -> T.Text -> IO Scheme
doExpr env expr = runScheme (evalSchemeText expr) env

debug :: String -> IO Scheme
debug = doExpr defaultEnv . T.pack

{-# LANGUAGE OverloadedStrings #-}
module Language.Scheme.Internal.Eval
    ( eval
    , runScheme
    , debugEval
    , unwrapOuterForm
    ) where

import           Control.Exception
import           Control.Monad.Reader
import qualified Data.Map                        as Map
import           Data.Semigroup                  ((<>))
import qualified Data.Text                       as T
import           Language.Scheme.Internal.AST
import           Language.Scheme.Internal.Core   (defaultEnv)
import qualified Language.Scheme.Internal.Parser as P

import Text.Parsec.Error(ParseError)

unwrapOuterForm :: Scheme -> Scheme
unwrapOuterForm (List [List x]) = (List x)
unwrapOuterForm x = x

debugEval :: Scheme -> Eval Scheme
debugEval x = do
  liftIO . print $ "Eval: step..."
  liftIO . print $ x
  eval x

-- Unwind a set of let forms in a tuple list. This
-- is unsafe and will throw if the forms are not
-- the expected types etc
unwindLetForms :: [Scheme] -> Eval [(T.Text, Scheme)]
unwindLetForms [] = return []
unwindLetForms ((Atom x):y:xs) = do
  y' <- eval y
  rest <- unwindLetForms xs
  return $ (x, y') : rest
unwindLetForms (x:xs) =
  throw $ TypeException msg
  where msg = "Expecting let binding first form to be atomic"

eval :: Scheme -> Eval Scheme
eval (Number i)                 = return (Number i)
eval (String s)                 = return (String s)
eval (Bool b)                   = return (Bool b)
eval (List [])                  = return Nil
eval (Nil)                      = return Nil
eval (Atom a)                   = getVar a
-- Quoted values
eval (List [Atom "print", val]) = do
  res <- eval val
  liftIO . print . show . showVal $ res
  return Nil
eval (List [Atom "quote", val]) = return val
-- If statements
eval (List [Atom "if", predicate, t, f]) = do
   result <- eval predicate
   case result of
     (Bool True)  -> eval t
     (Bool False) -> eval f
     _            -> throw $ GenericException "Expected boolean clause in if"
-- (define k=v)
eval (List [Atom "define", k, v]) = do
  env     <- ask
  case k of
    (Atom x) -> do
        v' <- eval v
        local (const $ Map.insert x v env) (return v')
    _ -> throw $ TypeException "Expecting atom"
-- (lambda (x) (+ x y))
eval (List [Atom "lambda", List params, expr]) = do
  boundLocalEnv <- ask
  return expr
--  return $ Lambda (Thunk
-- (let (x 10 y 20) (+ x y))
eval (List [Atom "let", (List boundVars), expr]) = do
  env <- ask
  forms <- unwindLetForms boundVars
  if (length forms == 0)
    then throw $ GenericException "Emtpy let binding"
    else local (const $ Map.fromList forms <> env) (eval expr)
-- This state represents simple function application.
-- We lookup the first argument (f xs) and then apply
-- the evaluated inner fn to the evaluated args
eval v@(List [x]) = eval x
eval v@(List (x:xs)) = do
--  liftIO . print $ v
  -- The scheme function to apply
  sf     <- eval x
  -- The arguments to apply a function to
  args   <- mapM eval xs
  case sf of
    (Fun (Thunk f)) -> f args
    x -> do
      throw $ GenericException $ "Not a function " <> (T.pack . show $ x)

eval _ = throw $ GenericException "Unbound eval form"

getVar :: T.Text -> Eval Scheme
getVar atom = do
  env <- ask
  case Map.lookup atom env of
    Just x  -> return x
    Nothing -> throw $ UnboundVar atom

-----------------------------------------------

runScheme :: Eval a -> EnvCtx -> IO a
runScheme expr env = runReaderT (unEval expr) env

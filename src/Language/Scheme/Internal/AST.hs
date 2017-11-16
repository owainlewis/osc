{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Language.Scheme.Internal.AST
    ( Scheme(..)
    , Eval(..)
    , IFunc(..)
    , EnvCtx
    , SchemeException(..)
    ) where

import qualified Data.Text            as T

import           Control.Exception
import           Control.Monad.Reader
import qualified Data.Map             as Map
import           Data.Semigroup       ((<>))
import           Data.Typeable        (Typeable)

type EnvCtx = Map.Map T.Text Scheme

newtype Eval a =
    Eval { unEval :: ReaderT EnvCtx IO a }
    deriving (Monad,
              Functor,
              Applicative,
              MonadReader EnvCtx,
              MonadIO)

data Scheme
  = Atom T.Text
  | List [Scheme]
  -- TODO add support for floating point etc
  | Number Integer
  | String T.Text
  | Nil
  | Bool Bool
  | Fun IFunc
  | Lambda IFunc EnvCtx
  deriving (Typeable, Eq)

data IFunc = IFunc { fn :: [Scheme] -> Eval Scheme }

instance Eq IFunc where
    (==) _ _ = False

instance Show Scheme where
    show = T.unpack . showAST

showVal :: Scheme -> T.Text
showVal val =
  case val of
    (Atom atom)     -> atom
    (String txt)    -> T.concat [ "\"" , txt, "\""]
    (Number num)    -> T.pack $ show num
    (Bool True)     -> "#t"
    (Bool False)    -> "#f"
    Nil             -> "'()"
    (List contents) -> "(" <> showVals showVal contents <> ")"
    (Fun _ )        -> "(internal f)"
    (Lambda _ _)    -> "(lambda f)"

wrapAST :: T.Text -> Scheme -> T.Text
wrapAST x v = T.concat [x, "[", showVal v, "]"]

showAST :: Scheme -> T.Text
showAST val =
  case val of
    x@(Atom atom)     -> wrapAST "Atom" x
    x@(String txt)    -> wrapAST "String" x
    x@(Number num)    -> wrapAST "Number" x
    x@(Bool True)     -> wrapAST "Bool" x
    x@(Bool False)    -> wrapAST "Bool" x
    x@(Nil)           -> wrapAST "Nil" x
    x@(List contents) ->
      let inner = showVals showAST contents in
      "List[" <> inner <> "]"

    x@(Fun _ )        -> "(internal f)"
    x@(Lambda _ _)    -> "(lambda f)"

showVals :: (a -> T.Text) -> [a] -> T.Text
showVals f = T.unwords . (map f)

data SchemeException
  = UnboundVar T.Text
  | ArityException Int Int -- (Actual, Expected)
  | ArgumentException T.Text
  | TypeException T.Text
  | GenericException T.Text
  deriving (Typeable)

instance Exception SchemeException

instance Show SchemeException where
  show = T.unpack . showError

showError :: SchemeException -> T.Text
showError err =
  case err of
    (UnboundVar e) -> T.concat ["Unbound variable: ", e]
    (TypeException e) -> T.concat ["Type error: ", e]
    (ArgumentException e) -> T.concat ["Argument error: ", e]
    (ArityException x y) ->
        T.concat ["Invalid arity: ", T.pack . show $ y]
    (GenericException e) -> T.concat ["Exception: ", e]

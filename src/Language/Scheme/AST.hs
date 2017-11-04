{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Language.Scheme.AST
    ( Scheme(..)
    , Eval(..)
    , EnvCtx
    , SchemeException(..)
    ) where

import qualified Data.Text            as T
import           Data.Typeable        (Typeable)

import           Control.Exception
import           Control.Monad.Reader
import qualified Data.Map             as Map

type EnvCtx = Map.Map T.Text Scheme

newtype Eval a = Eval { unEval :: ReaderT EnvCtx IO a }
  deriving (Monad,
            Functor,
            Applicative,
            MonadReader EnvCtx,
            MonadIO)

data Scheme
  = Atom T.Text
  | List [Scheme]
  | Number Integer
  | String T.Text
  | Fun IFunc
  | Lambda IFunc EnvCtx
  | Nil
  | Bool Bool
  deriving (Typeable, Eq)

data IFunc = IFunc { fn :: [Scheme] -> Eval Scheme }

instance Eq IFunc where
    (==) _ _ = False

instance Show Scheme where
    show = T.unpack . showVal

showVal :: Scheme -> T.Text
showVal val =
  case val of
    (Atom atom)     -> atom
    (String txt)    -> T.concat [ "\"" , txt, "\""]
    (Number num)    -> T.pack $ show num
    (Bool True)     -> "#t"
    (Bool False)    -> "#f"
    Nil             -> "'()"
    (List contents) -> "[?]"
    (Fun _ )        -> "(internal f)"
    (Lambda _ _)    -> "(lambda f)"

data SchemeException
  = UnboundVar T.Text
  deriving (Typeable)

instance Exception SchemeException

instance Show SchemeException where
  show = T.unpack . showError

showError :: SchemeException -> T.Text
showError err =
  case err of
    (UnboundVar e) -> T.concat ["Unbound variable: ", e]

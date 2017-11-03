{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Language.Scheme.Scheme where

import qualified Data.Text            as T
import           Data.Typeable        (Typeable)

import           Control.Exception
import           Control.Monad.Reader
import qualified Data.Map             as Map

type EnvCtx = Map.Map T.Text LispVal

newtype Eval a = Eval { unEval :: ReaderT EnvCtx IO a }
  deriving (Monad,
            Functor,
            Applicative,
            MonadReader EnvCtx,
            MonadIO)

data LispVal
  = Atom T.Text
  | List [LispVal]
  | Number Integer
  | String T.Text
  | Fun IFunc
  | Lambda IFunc EnvCtx
  | Nil
  | Bool Bool
  deriving (Typeable, Eq)

data IFunc = IFunc { fn :: [LispVal] -> Eval LispVal }

instance Eq IFunc where
    (==) _ _ = False

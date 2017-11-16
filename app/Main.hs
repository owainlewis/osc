module Main
  ( main
  ) where

import qualified Language.Scheme.Scheme as Scm

scratch :: FilePath
scratch = "scratch.scm"

main :: IO ()
main = Scm.executeFile scratch >>= print

debug :: IO ()
debug = Scm.executeFileAST scratch >>= print

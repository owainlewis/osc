module Main
  ( main
  ) where

import qualified Language.Scheme.Scheme as S

scratch :: FilePath
scratch = "scratch.scm"

main :: IO ()
main = do
  S.compileFile scratch >>= print >> return ()

module Main where

import qualified Language.Scheme.Scheme as Scm

main :: IO ()
main = Scm.executeFile "scratch.scm" >>= print

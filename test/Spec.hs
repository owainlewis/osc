{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Language.Scheme.Internal.AST
import           Language.Scheme.Internal.Parser
import           Test.Hspec

main :: IO ()
main = do
    hspec $ describe "Parsing" $ do
        it "should parse an Atom" $
            readExpr "foo+?" `shouldBe` (Right $ Atom "foo+?")

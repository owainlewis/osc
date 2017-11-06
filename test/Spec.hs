{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Language.Scheme.Internal.AST
import           Language.Scheme.Internal.Parser
import           Test.Hspec

import qualified Data.Text                       as T

main :: IO ()
main = do
    hspec $ describe "Parsing" $ do
        it "should parse an Atom" $
            readExpr "foo+?" `shouldBe` (Right $ Atom "foo+?")
        it "should parse a Number (positive integer)" $
            readExpr "10" `shouldBe` (Right $ Number 10)
        it "should parse boolean True" $
            readExpr "#t" `shouldBe` (Right $ Bool True)
        it "should parse boolean False" $
            readExpr "#f" `shouldBe` (Right $ Bool False)
        it "should parse Nil" $
            readExpr "'()" `shouldBe` (Right $ Nil)
        it "should parse a homogeneous list" $
            readExpr "(1 2 3)" `shouldBe` (Right $ List [Number 1, Number 2, Number 3])

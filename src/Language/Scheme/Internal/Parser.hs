{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
module Language.Scheme.Internal.Parser
    ( readExpr
    , readExprs
    , readSchemeFile
    ) where

import qualified Data.Text                    as T
import qualified Data.Text.IO                 as TIO
import           Text.Parsec
import qualified Text.Parsec.Language         as Lang
import           Text.Parsec.Text
import qualified Text.Parsec.Token            as Tok

import           Control.Monad                (mzero)
import           Data.Functor.Identity        (Identity)
import           Language.Scheme.Internal.AST

lexer :: Tok.GenTokenParser T.Text () Identity
lexer = Tok.makeTokenParser style

style :: Tok.GenLanguageDef T.Text () Identity
style = Lang.emptyDef {
    Tok.commentStart = "{-"
  , Tok.commentEnd = "-}"
  , Tok.commentLine = ";"
  , Tok.opStart = mzero
  , Tok.opLetter = mzero
  , Tok.identStart = letter <|> oneOf "!$%&*/:<=>?^_~"
  , Tok.identLetter = digit <|> letter <|> oneOf "!$%&*/:<=>?^_~+-.@"
  }

parens :: Parser a -> Parser a
parens = Tok.parens lexer

whitespace :: Parser ()
whitespace = Tok.whiteSpace lexer

lexeme :: Parser a -> Parser a
lexeme = Tok.lexeme lexer

parseIdentifier :: Parser T.Text
parseIdentifier = T.pack <$> (Tok.identifier lexer <|> specialIdentifier) <?> "identifier"
  where
    specialIdentifier :: Parser String
    specialIdentifier = lexeme $ try $ string "-" <|> string "+" <|> string "..."

parseQuoted :: Parser a -> Parser a
parseQuoted p = try (char '\'') *> p

parseNil :: Parser ()
parseNil = try ((char '\'') *> string "()") *> return () <?> "nil"

parseInteger :: Parser Integer
parseInteger = Tok.decimal lexer

parseBoolean :: Parser Scheme
parseBoolean = char '#'
    *> (char 't' *> return (Bool True)
    <|> char 'f' *> return (Bool False))

scheme :: Parser Scheme
scheme = parseBoolean <|> Nil <$ parseNil
     <|> Atom <$> parseIdentifier
     <|> Number <$> parseInteger
     <|> (\v -> List [Atom "quote", v]) <$> parseQuoted scheme
     <|> List <$> parens schemeList

schemeList :: Parser [Scheme]
schemeList = scheme `sepBy` whitespace

contents :: Parser a -> Parser a
contents p = whitespace *> lexeme p <* eof

readExpr :: T.Text -> Either ParseError Scheme
readExpr = parse (contents scheme) "<stdin>"

readExprs :: T.Text -> Either ParseError [Scheme]
readExprs = parse (contents schemeList) "<stdin>"

-- | Read the contents as a file and either return the parsed AST or an error
readSchemeFile :: FilePath -> IO (Either ParseError [Scheme])
readSchemeFile f = readExprs <$> TIO.readFile f

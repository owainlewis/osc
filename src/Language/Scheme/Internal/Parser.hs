{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
module Language.Scheme.Internal.Parser
    ( readExpr
    , readExprs
    , readExprOrThrow
    , readExprsOrThrow
    , parseFile
    , parseText
    ) where

import           Control.Monad                (mzero)
import           Control.Exception(throw)
import           Data.Functor.Identity        (Identity)
import qualified Data.Text                    as T
import           Language.Scheme.Internal.AST
import           Text.Parsec
import qualified Text.Parsec.Language         as Lang
import           Text.Parsec.Text
import qualified Text.Parsec.Token            as Tok

import qualified Data.Text.IO as TIO

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

parseIdentifier :: Parser Scheme
parseIdentifier = Atom <$> p
  where
    p = T.pack <$> (Tok.identifier lexer <|> specialIdentifier) <?> "identifier"
    specialIdentifier :: Parser String
    specialIdentifier =
        lexeme $ try $ string "-" <|> string "+" <|> string "..."

parseQuotedList :: Parser Scheme
parseQuotedList =
  (\v -> List [Atom "quote", v]) <$> (try (char '\'') *> scheme)

sign :: Parser (Integer -> Integer)
sign = char '-' *> return negate
   <|> char '+' *> return id
   <|> return id

parseNumber :: Parser Scheme
parseNumber = Number <$> (sign <*> Tok.decimal lexer)

parseNil :: Parser Scheme
parseNil = Nil <$ p
    where p = try ((char '\'') *> string "()") *> return () <?> "nil"

parseBoolean :: Parser Scheme
parseBoolean = char '#'
    *> (char 't' *> return (Bool True)
    <|> char 'f' *> return (Bool False))

parseString :: Parser Scheme
parseString = String <$> p
    where p = T.pack <$> Tok.stringLiteral lexer

parseList = List <$> parens schemeList

scheme :: Parser Scheme
scheme = parseBoolean
     <|> parseNil
     <|> try parseNumber
     <|> parseIdentifier
     <|> parseString
     <|> parseQuotedList
     <|> parseList

schemeList :: Parser [Scheme]
schemeList = scheme `sepBy` whitespace

contents :: Parser a -> Parser a
contents p = whitespace *> lexeme p <* eof

throwLeft :: Either ParseError a -> a
throwLeft = either (throw . GenericException . T.pack . show) id

-- | Read a single expression from text
--
-- This would typically be used when reading in a REPL context
--
-- @
-- λ> readExpr "" (T.pack "(+ 1 2 3)")
-- @
--
readExpr :: SourceName -> T.Text -> Either ParseError Scheme
readExpr source = parse (contents scheme) source

-- | Read multiple expressions.
--
-- Typically this would be used when reading from a file
--
-- @
-- λ> readExprs "" (T.pack "(+ 1 2 3)")
-- @
readExprs :: SourceName -> T.Text -> Either ParseError Scheme
readExprs = parse (contents $ List <$> schemeList)

-- | Read a single expr but throw if we encounter a parse error
--
readExprOrThrow :: SourceName -> T.Text -> Scheme
readExprOrThrow source = throwLeft . readExpr source

-- | Read multiple expressions but throw if we encounter a
--   parse error
readExprsOrThrow :: SourceName -> T.Text -> Scheme
readExprsOrThrow source = throwLeft . readExprs source

-- | Parse file will parse a file from the filesystem into an
--   AST. If the parser encounters an error it will throw early.
--   Unwinding the either at this stage reduces complexity
--   elsewhere.
parseFile :: FilePath -> IO Scheme
parseFile path =
    (readExprsOrThrow "<IO>") <$> (TIO.readFile path)

parseText :: T.Text -> Scheme
parseText = readExprsOrThrow "<std>"

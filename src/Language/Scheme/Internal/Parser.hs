{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
module Language.Scheme.Internal.Parser
    ( readExpr
    , readExprs
    , readExprsFromFile
    ) where

import           Control.Monad                (mzero)
import           Data.Functor.Identity        (Identity)
import qualified Data.Text                    as T
import           Language.Scheme.Internal.AST
import           Text.Parsec
import qualified Text.Parsec.Language         as Lang
import           Text.Parsec.Text
import qualified Text.Parsec.Token            as Tok

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
     -- We need to try here because of parsing (+1) and (+ 1)
     <|> try parseNumber
     <|> parseIdentifier
     <|> parseString
     <|> parseQuotedList
     <|> parseList

schemeList :: Parser [Scheme]
schemeList = scheme `sepBy` whitespace

contents :: Parser a -> Parser a
contents p = whitespace *> lexeme p <* eof

readExpr :: T.Text -> Either ParseError Scheme
readExpr = parse (contents scheme) "<stdin>"

readExprs :: T.Text -> Either ParseError [Scheme]
readExprs = parse (contents schemeList) "<stdin>"

-- | Given the contents of a file, read the contents into
--   a single scheme list of exprs
readExprsFromFile :: T.Text -> Either ParseError Scheme
readExprsFromFile = parse (contents $ List <$> schemeList) "<fio>"

module Language.Scheme.Parser where

import qualified Data.Text             as T
import           Text.Parsec
import qualified Text.Parsec.Language  as Lang
import           Text.Parsec.Text
import qualified Text.Parsec.Token     as Tok

import           Control.Monad         (mzero)
import           Data.Functor.Identity (Identity)
import           Language.Scheme.AST

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

identifier :: Parser T.Text
identifier = T.pack <$> (Tok.identifier lexer <|> specialIdentifier) <?> "identifier"
  where
    specialIdentifier :: Parser String
    specialIdentifier = lexeme $ try $ string "-" <|> string "+" <|> string "..."

nil :: Parser ()
nil = try ((char '\'') *> string "()") *> return () <?> "nil"

parser = Nil <$ nil
       <|> Atom <$> identifier

contents p = whitespace *> lexeme p <* eof

readExpr :: T.Text -> Either ParseError Scheme
readExpr = parse (contents parser) "<stdin>"

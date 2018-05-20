{-# LANGUAGE OverloadedStrings #-}

module Parser
    ( parseProgram
    , ParsingException (..)
    ) where

import           Expression                 (Expr (..))
import           Statement                  (Statement (..))

import           Text.Megaparsec
import           Text.Megaparsec.Byte       (alphaNumChar, char, eol,
                                             letterChar, string)
import qualified Text.Megaparsec.Byte.Lexer as L
import           Text.Megaparsec.Expr

import           Control.Applicative        (empty)
import           Control.Monad.Catch        (Exception, MonadThrow, throwM)

import           Data.Void

import qualified Data.ByteString            as ByteStr
import qualified Data.ByteString.Internal   as ByteStr (c2w)
import qualified Data.ByteString.UTF8       as ByteStr8

type Str = ByteStr8.ByteString
type Parser = Parsec Void Str

newtype ParsingException = ParsingException (ParseError (Token Str) Void)
instance Show ParsingException where
    show (ParsingException e) = parseErrorPretty e
instance Exception ParsingException

space1 :: Parser ()
space1 = skipSome (char (ByteStr.c2w ' '))

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Str -> Parser Str
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integer :: Parser Int
integer = lexeme L.decimal

reservedWord :: Str -> Parser ()
reservedWord w = lexeme (string w *> notFollowedBy alphaNumChar)

identifier :: Parser Str
identifier = (lexeme . try) (pack >>= validate)
  where
    pack       = ByteStr.pack <$> ((:) <$> letterChar <*> many alphaNumChar)
    validate x = if x `elem` reserved
                 then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                 else return x
    reserved :: [Str]
    reserved = ["let", "mut", "for"]

termParser :: Parser Expr
termParser =
    (Let <$> (symbol "(" *> reservedWord "let" *> (ByteStr8.toString <$> identifier) <* symbol "=")
        <*> (exprParser <* symbol "in" ) <*> (exprParser <* symbol ")"))
    <|> Var <$> (ByteStr8.toString <$> identifier)
    <|> Lit <$> integer
    <|> parens exprParser

exprParser :: Parser Expr
exprParser = makeExprParser termParser operators
  where
    operators :: [[Operator Parser Expr]]
    operators = [
        [InfixL (Mul <$ symbol "*"), InfixL (Div <$ symbol "/")],
        [InfixL (Add <$ symbol "+"), InfixL (Sub <$ symbol "-")]
        ]

stmtParser :: Parser Statement
stmtParser = sc *> (
    Def <$> (reservedWord "mut" *> (ByteStr8.toString <$> identifier) <* symbol "=") <*> exprParser
    <|> Assignement <$> ((ByteStr8.toString <$> identifier) <* symbol "=") <*> exprParser
    <|> Print <$> (symbol "<" *> exprParser)
    <|> Read <$> (symbol ">" *> (ByteStr8.toString <$> identifier))
    <|> do
        _ <- reservedWord "for" *> symbol "("
        ini <- exprParser
        _ <- reservedWord "to"
        end <- exprParser
        _ <- symbol ")" *> symbol "{" *> eol
        body <- stmtsParser
        _ <- symbol "}"
        return $ Loop ini end body
    )

stmtsParser :: Parser [Statement]
stmtsParser = sc *> many (stmtParser <* eol)

parseProgram :: (MonadThrow m) => String -> String -> m [Statement]
parseProgram name input =
    either (throwM . ParsingException) return (parse stmtsParser name (ByteStr8.fromString input))

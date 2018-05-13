module Parser
    ( parseProgram
    , ParsingException (..)
    ) where

import           Expression                 (Expr (..))
import           Statement                  (Statement (..))

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Expr

import           Control.Applicative        (empty)
import           Control.Monad.Catch        (Exception, MonadThrow, throwM)

import           Data.Void

type Parser = Parsec Void String

newtype ParsingException = ParsingException (ParseError (Token String) Void)

instance Show ParsingException where
    show (ParsingException e) = parseErrorPretty e

instance Exception ParsingException

space1 :: Parser ()
space1 = skipSome $ char ' '

sc :: Parser ()
sc = L.space Parser.space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integer :: Parser Int
integer = lexeme L.decimal

rword :: String -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x
    rws = ["let", "mut"]

termParser :: Parser Expr
termParser = (Let <$> (symbol "(" *> rword "let" *> identifier <* symbol "=")
    <*> (exprParser <* symbol "in" ) <*> (exprParser <* symbol ")"))
    <|> Var <$> identifier
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
    Def <$> (rword "mut" *> identifier <* symbol "=") <*> exprParser
    <|> Assignement <$> (identifier <* symbol "=") <*> exprParser
    <|> Print <$> (symbol "<" *> exprParser)
    <|> Read <$> (symbol ">" *> identifier)
    )

stmtsParser :: Parser [Statement]
stmtsParser = sc *> many (stmtParser <* eol)

parseProgram :: (MonadThrow m) => String -> String -> m [Statement]
parseProgram name s = either (throwM . ParsingException) return (parse stmtsParser name s)

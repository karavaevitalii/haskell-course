module Parsers
    ( parseCBS
    , parseInteger
    ) where

import           Control.Applicative (many, some, (<|>))
import           Data.Char           (digitToInt, isDigit)
import           ParserCombinators

parseCBS :: Parser Char String
parseCBS = impl <* eof
  where
    impl :: Parser Char String
    impl = concat <$> many ((\a l b -> a:l ++ [b]) <$> element '(' <*> impl <*> element ')')

parseInteger :: Parser Char Int
parseInteger = ((((-1) *) <$ element '-') <|> id <$ element '+' <|> id <$ ok) <*>
    (foldl1 (\acc x -> acc * 10 + x) <$> some (digitToInt <$> satisfy isDigit))


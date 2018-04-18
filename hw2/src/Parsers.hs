module Parsers
    ( parseCBS
    , parseInteger
    , parseList
    ) where

import           Control.Applicative (many, some, (<|>))
import           Data.Char           (digitToInt, isDigit, isSpace)
import           ParserCombinators

removeWhitespaces :: Parser Char String
removeWhitespaces = many $ satisfy isSpace

parseCBS :: Parser Char String
parseCBS = impl <* eof
  where
    impl :: Parser Char String
    impl = removeWhitespaces *> 
        (concat <$> many ((\a l b -> a:l ++ [b]) <$> element '(' <*> impl <*> element ')'))
        <* removeWhitespaces

parseInteger :: Parser Char Int
parseInteger = removeWhitespaces *> 
    ((((-1) *) <$ element '-') <|> id <$ element '+' <|> id <$ ok) <*>
    (foldl1 (\acc x -> acc * 10 + x) <$> some (digitToInt <$> satisfy isDigit))
    <* removeWhitespaces

parseList :: Parser Char [[Int]]
parseList = ((:) <$> listP <*> many (element ',' *> listP)) <|> const [] <$> ok 
  where
    parseN :: Int -> Parser Char [Int]
    parseN 1 = return <$> parseInteger
    parseN i = (:) <$> (parseInteger <* element ',') <*> parseN (i - 1)
    listP :: Parser Char [Int]
    listP = (parseInteger <* element ',') >>= parseN

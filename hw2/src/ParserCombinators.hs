{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase   #-}

module ParserCombinators
    ( Parser (runParser)
    , ok
    , eof
    , satisfy
    , element
    , stream
    ) where

import           Control.Applicative (Alternative (..))
import           Control.Monad       ((>=>))
import           Data.Bifunctor      (first)
import           Data.List

newtype Parser s a = Parser { runParser :: [s] -> Maybe (a, [s]) }

instance Functor (Parser s) where
    fmap :: (a -> b) -> Parser s a -> Parser s b
    fmap f (Parser p) = Parser (fmap (first f) . p)

instance Applicative (Parser s) where
    pure :: a -> Parser s a
    pure a = Parser $ \s -> Just (a, s)

    (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
    Parser pf <*> Parser pa =
        Parser $ pf >=> \(f, t) -> pa t >>= \(a, r) -> Just (f a, r)

instance Monad (Parser s) where
    return :: a -> Parser s a
    return = pure

    (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
    Parser p >>= next = Parser $ \s -> case p s of
        Just (res, rest) -> runParser (next res) rest
        _                -> Nothing

instance Alternative (Parser s) where
    empty :: Parser s a
    empty = Parser (const Nothing)

    (<|>) :: Parser s a -> Parser s a -> Parser s a
    Parser p1 <|> Parser p2 = Parser $ \s -> p1 s <|> p2 s

ok :: Parser s ()
ok = Parser $ \s -> Just ((), s)

eof :: Parser s ()
eof = Parser $ \case
    []  -> Just ((), [])
    _   -> Nothing

satisfy :: (s -> Bool) -> Parser s s
satisfy p = Parser $ \case
    [] -> Nothing
    (x:xs) -> if p x then Just (x, xs) else Nothing

element :: Eq s => s -> Parser s s
element s = satisfy (== s)

stream :: Eq s => [s] -> Parser s [s]
stream prefix = Parser $ \s -> if prefix `isPrefixOf` s
                                then Just (prefix, snd $ splitAt (length prefix) s)
                                else Nothing

{-# LANGUAGE InstanceSigs #-}

module Folding
  ( Pair (..)
  , NonEmpty (..)
  , testSplitOn
  , testPair
  , testNonEmpty
  ) where

data Pair a = Pair a a

data NonEmpty a = a :| [a]

instance Foldable Pair where
  foldMap :: Monoid m => (a -> m) -> Pair a -> m
  foldMap f (Pair x y) = f x `mappend` f y

  foldr :: (a -> b -> b) -> b -> Pair a -> b
  foldr f z (Pair x y) = f x $ f y z

instance Foldable NonEmpty where
  foldMap :: Monoid m => (a -> m) -> NonEmpty a -> m
  foldMap f (x :| xs) = f x `mappend` foldMap f xs

  foldr :: (a -> b -> b) -> b -> NonEmpty a -> b
  foldr f z (x :| xs) = x `f` foldr f z xs

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn x = foldr f [[]]
  where
    f item (y:ys)
      | item == x = [] : y : ys
      | otherwise = (item : y) : ys
    f _ [] = undefined

testSplitOn :: Bool
testSplitOn = splitOn '/' "path/to/file" == ["path", "to", "file"]

testPair :: Bool
testPair = foldr (++) [] (Pair [1] [2]) == [1,2]

testNonEmpty :: Bool
testNonEmpty = foldr (+) 5 (6 :| [1,2,3,4,5]) == 26

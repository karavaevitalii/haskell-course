module Monoids
  ( maybeConcat
  , testMaybeConcat
  ) where

import           Data.Semigroup (Semigroup (..))

maybeConcat :: [Maybe [a]] -> [a]
maybeConcat = foldr impl []
  where
    impl Nothing z     = z
    impl (Just list) z = list ++ z

testMaybeConcat :: Bool
testMaybeConcat = maybeConcat [Just [1,2,3], Nothing, Just [4,5]] == [1,2,3,4,5]

data NonEmpty a =
  a :| [a]

instance Semigroup (NonEmpty t) where
  (x :| xs) <> (y :| ys) = x :| (xs ++ [y] ++ ys)


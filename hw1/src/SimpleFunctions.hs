module SimpleFunctions
  ( order3
  , smartReplicate
  , contains
  , stringSum
  ) where

import           Data.List (sort)

order3 :: Ord a => (a, a, a) -> (a, a, a)
order3 (a, b, c) = (x, y, z)
  where [x, y, z] = sort [a, b, c]

smartReplicate :: [Int] -> [Int]
smartReplicate = concatMap (\x -> replicate x x)

contains :: Eq a => a -> [[a]] -> [[a]]
contains = filter . elem

stringSum :: String -> Int
stringSum = sum . map read . words

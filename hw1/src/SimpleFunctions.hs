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

--testOrder :: Bool
--testOrder = order3 (3, 2, 1) == (1, 2, 3) && order3 (1, 3, 2) == (1, 2, 3) &&
--  order3 (1, 2, 3) == (1, 2, 3)
--
--testSmartReplicate :: Bool
--testSmartReplicate = smartReplicate [1, 2, 3] == [1, 2, 2, 3, 3, 3]
--
--testContains :: Bool
--testContains = contains 1 [[1,2,3], [1..5], [2,3,4]] == [[1,2,3],[1..5]]
--
--testStringSum :: Bool
--testStringSum = stringSum " 1 2 3 4 5\t  6" == 21

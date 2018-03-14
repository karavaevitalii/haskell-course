module SimpleFunctions
  ( order3
  , smartReplicate
  , contains
  , stringSum
  , checkOrder
  , checkSmartReplicate
  , checkContains
  , checkStringSum
  ) where

import           Data.List (sort)

order3 :: Ord a => (a, a, a) -> (a, a, a)
order3 (a, b, c) = (x, y, z)
  where [x, y, z] = sort [a, b, c]

smartReplicate :: [Int] -> [Int]
smartReplicate = concat . map (\x -> replicate x x)

contains :: Eq a => a -> [[a]] -> [[a]]
contains = filter . elem

stringSum :: String -> Int
stringSum = sum . (map read) . words

checkOrder :: Bool
checkOrder = order3 (3, 2, 1) == (1, 2, 3) && order3 (1, 3, 2) == (1, 2, 3) &&
  order3 (1, 2, 3) == (1, 2, 3)

checkSmartReplicate :: Bool
checkSmartReplicate = smartReplicate [1, 2, 3] == [1, 2, 2, 3, 3, 3]

checkContains :: Bool
checkContains = contains 1 [[1,2,3], [1..5], [2,3,4]] == [[1,2,3],[1..5]]

checkStringSum :: Bool
checkStringSum = stringSum " 1 2 3 4 5\t  6" == 21

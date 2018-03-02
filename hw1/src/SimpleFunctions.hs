module SimpleFunctions
  ( order3
  , smartReplicate
  , contains
  , stringSum
  ) where

order3 :: (Ord a) => (a, a, a) -> (a, a, a)
order3 (a, b, c) = (min a (min b c), max a (min b c), max a (max b c))

smartReplicate :: [Int] -> [Int]
smartReplicate = concat . map (\x -> replicate x x)

contains :: (Foldable t, Eq a) => a -> [t a] -> [t a]
contains = filter . elem

stringSum :: String -> Int
stringSum = sum . (map read) . words
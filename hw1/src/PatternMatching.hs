module PatternMatching
  ( removeAt
  , mergeSort
  ) where

import Data.List (splitAt)

removeAt :: Int -> [a] -> a
removeAt idx l = head $ snd $ splitAt idx l

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort (left xs)) (mergeSort (right xs))
  where
    left ys  = take (length ys `div` 2) ys
    right ys = drop (length ys `div` 2) ys

    merge :: Ord a => [a] -> [a] -> [a]
    merge [] ys         = ys
    merge ys []         = ys
    merge l@(y:ys) r@(z:zs) = if y < z then y:merge ys r else z:merge zs l
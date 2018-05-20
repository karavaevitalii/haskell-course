module PatternMatching
  ( removeAt
  , mergeSort
  ) where

import           Data.List (splitAt)

removeAt :: Int -> [a] -> (Maybe a, [a])
removeAt idx lst
    | idx < 0 = (Nothing, lst)
    | otherwise = let (before, r) = splitAt idx lst in
        case r of
        []        -> (Nothing, lst)
        (x:after) -> (Just x, before ++ after)

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
    merge l@(y:ys) r@(z:zs) = if y < z
                            then y:merge ys r
                            else z:merge zs l


module Main where

import           Criterion.Main

import qualified Data.List      as L
import qualified Data.Map       as M
import qualified Data.Set       as S

nubStd :: Eq a => [a] -> [a]
nubStd = L.nub

nubStdSorted :: Ord a => [a] -> [a]
nubStdSorted = removeDuplicates . L.sort

nubSorted :: Ord a => [a] -> [a]
nubSorted = removeDuplicates . mergeSort
  where
    mergeSort :: Ord a => [a] -> [a]
    mergeSort []    = []
    mergeSort [x]   = [x]
    mergeSort xs    = let (left, right) = split xs
                      in merge (mergeSort left) (mergeSort right)

    split :: [a] -> ([a], [a])
    split []        = ([], [])
    split [x]       = ([x], [])
    split (x:y:xs)  = let (odds, evens) = split xs
                      in (x:odds, y:evens)

    merge :: Ord a => [a] -> [a] -> [a]
    merge [] ys             = ys
    merge ys []             = ys
    merge l@(y:ys) r@(z:zs) = if y <= z
                              then y:merge r ys
                              else z:merge l zs

nubSet :: Ord a => [a] -> [a]
nubSet = S.toList . S.fromList

nubMap :: Ord a => [a] -> [a]
nubMap = impl M.empty
  where
    impl :: Ord a => M.Map a Int -> [a] -> [a]
    impl _ [] = []
    impl m (y:ys)
        | M.member y m = impl m ys
        | otherwise = y:impl (M.insert y 1 m) ys

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates xs = reverse $ impl xs []
  where
    impl :: Eq a => [a] -> [a] -> [a]
    impl [] acc             = acc
    impl (y:ys) []          = impl ys [y]
    impl (y:ys) acc@(z:_)   = if y == z
                              then impl ys acc
                              else impl ys (y:acc)

main :: IO ()
main = defaultMain $
    let
    straight, reversed, repeated, shuffled :: [Int]
    straight = [1 .. 100]
    reversed = [100, 99 .. 1]
    repeated = [1 .. 10] ++ [1 .. 10] ++ [10, 9 .. 1] ++ [10, 9 .. 1] ++ [11 .. 100]
    shuffled = [-10 .. -1] ++ [20 .. 30] ++ [50 .. 60] ++ [15 .. 25] ++ [75,74 .. 65] ++
               [-11,-12 .. -20] ++ [30 .. 40] ++ [15, 14, 5] ++ [90 .. 100] ++ [37 .. 45]
    in
        [ bgroup "std"
            [ bench "straight" $ nf nubStd straight
            , bench "reversed" $ nf nubStd reversed
            , bench "repeated" $ nf nubStd repeated
            , bench "shuffled" $ nf nubStd shuffled
            ]
        , bgroup "stdSorted"
            [ bench "straight" $ nf nubStdSorted straight
            , bench "reversed" $ nf nubStdSorted reversed
            , bench "repeated" $ nf nubStdSorted repeated
            , bench "shuffled" $ nf nubStdSorted shuffled
            ]
        , bgroup "sorted"
            [ bench "straight" $ nf nubSorted straight
            , bench "reversed" $ nf nubSorted reversed
            , bench "repeated" $ nf nubSorted repeated
            , bench "shuffled" $ nf nubSorted shuffled
            ]
        , bgroup "set"
            [ bench "straight" $ nf nubSet straight
            , bench "reversed" $ nf nubSet reversed
            , bench "repeated" $ nf nubSet repeated
            , bench "shuffled" $ nf nubSet shuffled
            ]
        , bgroup "map"
            [ bench "straight" $ nf nubMap straight
            , bench "reversed" $ nf nubMap reversed
            , bench "repeated" $ nf nubMap repeated
            , bench "shuffled" $ nf nubMap shuffled
            ]
        ]

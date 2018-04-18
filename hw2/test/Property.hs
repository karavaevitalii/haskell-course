module Property where

import Hedgehog
import MonadicCalc (bin)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.List(sort)

genInt :: Gen Int
genInt = Gen.int (Range.linear 0 20)

prop_bin :: Property
prop_bin = property $ forAll genInt >>= \l ->
    let list = binToIntsList (bin l) in sort(list) === [0 .. (2^l - 1)]
  where
    binToIntsList :: [[Int]] -> [Int]
    binToIntsList = map $ foldr (\x acc -> x + acc * 2) 0

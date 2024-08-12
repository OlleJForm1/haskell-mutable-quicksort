module Main where

import           Data.List          (sort)
import           Data.Ord.Quicksort (quickSort)
import           Test.QuickCheck    as QC

sorts :: [Int] -> Bool
sorts xs = sort xs == quickSort xs

main :: IO ()
main = quickCheck $ sorts


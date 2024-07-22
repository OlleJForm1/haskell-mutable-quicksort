module Main where

import Test.QuickCheck as QC
import Data.Ord.Quicksort (quickSort)
import Data.List (sort)

sorts :: [Int] -> Bool
sorts xs = sort xs == quickSort xs

main :: IO ()
main = quickCheck $ sorts


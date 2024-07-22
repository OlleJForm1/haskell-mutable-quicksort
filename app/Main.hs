module Main where

import Data.Ord.Quicksort (quickSort)

main :: IO ()
main = print $ quickSort xs
  where xs = [1, 23, 5, 123, 4, 2, 34, 7, 4, 7] :: [Int]


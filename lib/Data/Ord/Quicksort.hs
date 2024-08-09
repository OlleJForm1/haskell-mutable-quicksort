{-# LANGUAGE ScopedTypeVariables #-}

module Data.Ord.Quicksort where

import Control.Monad.ST (ST)
import qualified Data.Vector.Mutable as VM
import Control.Monad (when, (>=>))
import Control.Monad.Loops (untilJust, untilM_)
import Data.STRef (newSTRef, modifySTRef, readSTRef)
import Data.Functor (($>))
import Control.Monad.Fix (fix)
import Data.Function ((&))
import Data.Vector.Mutable.Function (withSTVector)
import Control.Applicative.Bitraversable (bothA_, bothA)
import Data.Ord.Compare (lessOrEqualOn, greaterOrEqualOn)
import Data.Ord (comparing)

quickSort :: Ord a => [a] -> [a]
quickSort = quickSortBy compare

quickSortBy :: forall a. (a -> a -> Ordering) -> [a] -> [a]
quickSortBy c = withSTVector $ fix $ \rec v ->
    when (VM.length v > 1) $ do
        partition v >>= bothA_ rec
  where 
    partition :: VM.STVector s a -> ST s (VM.STVector s a, VM.STVector s a)
    partition a = do
        p <- VM.read a 0
        (l, h) <- newSTRef `bothA` (-1, VM.length a)
        untilJust $ do
            increment l `untilM_` (p `lessOrEqualOn` c) <$> (a `at` l)
            decrement h `untilM_` (p `greaterOrEqualOn` c) <$> (a `at` h)
            (l', h') <- readSTRef `bothA` (l, h)
            if l' < h'
              then VM.swap a l' h' $> Nothing
              else VM.splitAt (h' + 1) a & Just & pure

    increment = (`modifySTRef` (+   1))
    decrement = (`modifySTRef` (+ (-1)))
    at a = readSTRef >=> VM.read a

main :: IO ()
main = print $ quickSort ([1, 5, 3, 6, 7, 22, 4, 6, 2, 3, 44] :: [Int])


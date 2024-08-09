-- "Haskell is the finest imperative programming language"

{-# LANGUAGE ScopedTypeVariables #-}

module Data.Ord.Quicksort where

import Control.Monad.ST (ST)
import qualified Data.Vector.Mutable as VM
import Control.Monad (when, (>=>), void)
import Control.Monad.Loops (untilJust, untilM_)
import Data.STRef (newSTRef, modifySTRef, readSTRef)
import Data.Functor (($>))
import Data.Function ((&))
import Data.Function.Recursive (recursive)
import Data.Vector.Mutable.Function (withSTVector)
import Control.Applicative.Bitraversable (bothA_, bothA)
import Data.Ord.Compare (lessOrEqualOn, greaterOrEqualOn)
import Data.Ord (comparing)


quickSort :: (Show a, Ord a) => [a] -> [a]
quickSort = quickSortBy compare

-- Efficient, in place, recursive, imperative-style quicksort using Hoare's partition scheme
-- with a simple middle element pivot
quickSortBy :: forall a. Show a => (a -> a -> Ordering) -> [a] -> [a]
quickSortBy c = withSTVector $ recursive $ \rec v ->
    when (VM.length v > 1) $ do
        partition v >>= bothA_ rec
  where 
    partition :: VM.STVector s a -> ST s (VM.STVector s a, VM.STVector s a)
    partition a = do
        p <- pivot a
        (l, h) <- newSTRef `bothA` (-1, VM.length a)
        untilJust $ do
            increment l `untilM_` (p `lessOrEqualOn` c) <$> (a `at` l)
            decrement h `untilM_` (p `greaterOrEqualOn` c) <$> (a `at` h)
            (l', h') <- readSTRef `bothA` (l, h)
            if l' < h'
              then VM.swap a l' h' $> Nothing
              else VM.splitAt h' a & Just & pure

    increment = (`modifySTRef` (+   1))
    decrement = (`modifySTRef` (+ (-1)))
    at a = readSTRef >=> VM.read a
    pivot a = VM.read a (VM.length a `div` 2)


-- "Haskell is the finest imperative programming language"

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Data.Ord.Quicksort where

import           Control.Applicative.Bitraversable (bothA, bothA_)
import           Control.Monad                     (void, when, (>=>))
import           Control.Monad.Loops               (untilJust, untilM_)
import           Control.Monad.ST                  (ST)
import           Data.Function                     ((&))
import           Data.Function.Loops               (loopM)
import           Data.Function.Recursive           (recursive)
import           Data.Functor                      (($>))
import qualified Data.List                         as L
import           Data.Ord                          (comparing)
import           Data.Ord.Compare                  (greaterOrEqualOn,
                                                    lessOrEqualOn)
import           Data.STRef                        (modifySTRef, newSTRef,
                                                    readSTRef)
import           Data.Vector.Mutable               (STVector, length, read,
                                                    splitAt, swap)
import           Data.Vector.Mutable.Function      (mutableListTranform)
import           Prelude                           hiding (length, read,
                                                    splitAt)

qs :: Ord a => [a] -> [a]
qs [] = []
qs (x:xs) = qs lesser ++ qs greater
  where
    (lesser, greater) = L.partition (<= x) xs

quickSort :: Ord a => [a] -> [a]
quickSort = quickSortBy compare

-- Efficient, in place, recursive, imperative-style quicksort using Hoare's partition scheme
-- with a simple middle element pivot
quickSortBy :: ∀ a. (a -> a -> Ordering) -> [a] -> [a]
quickSortBy c = mutableListTranform $ recursive $ \recurse vector ->
    when (length vector > 1) $ do
        partition vector >>= bothA_ recurse
  where
    partition :: STVector s a -> ST s (STVector s a, STVector s a)
    partition vector = do
        p <- choosePivot vector
        ptrs@(low, high) <- newSTRef `bothA` (-1, length vector)
        loopM $ \continue done -> do
            increment low `untilM_` ((p `lessOrEqualOn` c) `than` (vector `at` low))
            decrement high `untilM_` ((p `greaterOrEqualOn` c) `than` (vector `at` high))
            (low', high') <- readSTRef `bothA` ptrs
            if low' < high'
              then swap vector low' high' *> continue
              else splitAt low' vector & done

    increment = (`modifySTRef` (+   1))
    decrement = (`modifySTRef` (+ (-1)))
    at a = readSTRef >=> read a
    choosePivot a = read a (length a `div` 2)
    than :: Functor f => (a -> b) -> f a -> f b
    than = (<$>)


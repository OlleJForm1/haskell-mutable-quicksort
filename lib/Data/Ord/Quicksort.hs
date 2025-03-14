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
import           Data.Vector.Mutable.Function      (withSTVector)
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
quickSortBy c = withSTVector $ recursive $ \rec v ->
    when (length v > 1) $ do
        partition v >>= bothA_ rec
  where
    partition :: STVector s a -> ST s (STVector s a, STVector s a)
    partition a = do
        p <- pivot a
        (l, h) <- newSTRef `bothA` (-1, length a)
        loopM $ \continue done -> do
            increment l `untilM_` ((p `lessOrEqualOn` c) `than` (a `at` l))
            decrement h `untilM_` ((p `greaterOrEqualOn` c) `than` (a `at` h))
            (l', h') <- readSTRef `bothA` (l, h)
            if l' < h'
              then swap a l' h' $> continue
              else splitAt l' a & done

    increment = (`modifySTRef` (+   1))
    decrement = (`modifySTRef` (+ (-1)))
    at a = readSTRef >=> read a
    pivot a = read a (length a `div` 2)
    than :: Functor f => (a -> b) -> f a -> f b
    than = (<$>)


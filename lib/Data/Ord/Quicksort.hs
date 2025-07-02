-- "Haskell is the finest imperative programming language"

{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Data.Ord.Quicksort
  ( qs
  , quickSort
  , quickSortBy
  , quickSortGeneral
  , hoarePartition
  , median3Pivot
  )
  where

import           Control.Applicative.Bitraversable (bothA, bothA_)
import           Control.Applicative.Tuple         (allT3A)
import           Control.Monad                     (when, (>=>))
import           Control.Monad.Loops               (untilM_)
import           Control.Monad.ST                  (ST)
import           Data.Function                     ((&))
import           Data.Function.Loops               (loopM)
import           Data.Function.Recursive           (recursive)
import qualified Data.List                         as L
import           Data.Ord.Compare                  (greaterOrEqualOn,
                                                    lessOrEqualOn)
import           Data.STRef                        (modifySTRef, newSTRef,
                                                    readSTRef)
import           Data.Vector.Mutable               (STVector, length, read,
                                                    splitAt, swap)
import           Data.Vector.Mutable.Function      (mutableListTransform)
import           Prelude                           hiding (length, read,
                                                    splitAt)

qs :: Ord a => [a] -> [a]
qs [] = []
qs (x:xs) = qs lesser ++ qs greater
  where
    (lesser, greater) = L.partition (<= x) xs


quickSort :: Ord a => [a] -> [a]
quickSort = quickSortBy compare


quickSortBy :: (a -> a -> Ordering) -> [a] -> [a]
quickSortBy c = quickSortGeneral $ hoarePartition c (median3Pivot c)


quickSortGeneral :: (∀ s. STVector s a -> ST s (STVector s a, STVector s a))
                 -> [a]
                 -> [a]
quickSortGeneral partition =
    mutableListTransform $ recursive $ \recurse vector ->
        when (length vector > 1) $
            partition vector >>= bothA_ recurse


hoarePartition :: (a -> a -> Ordering)
               -> (∀ s. STVector s a -> ST s a)
               -> STVector s a
               -> ST s (STVector s a, STVector s a)
hoarePartition comp pivot vector = do
    p <- pivot vector
    ptrs@(low, high) <- newSTRef `bothA` (-1, length vector)
    loopM $ \continue done -> do
        increment low `untilM_`
          ((p `lessOrEqualOn` comp) `than` (vector `at` low))

        decrement high `untilM_`
          ((p `greaterOrEqualOn` comp) `than` (vector `at` high))

        (low', high') <- readSTRef `bothA` ptrs
        if low' < high'
          then swap vector low' high' *> continue
          else splitAt low' vector & done

  where

    increment = (`modifySTRef` (+   1))
    decrement = (`modifySTRef` (+ (-1)))
    at a = readSTRef >=> read a
    than :: Functor f => (a -> b) -> f a -> f b
    than = (<$>)


median3Pivot :: (a -> a -> Ordering) -> STVector s a -> ST s a
median3Pivot comp vector = do
    (low, mid, high) <- allT3A
                          (read vector)
                          (0, length vector `div` 2, length vector - 1)
    pure $ median3 low mid high
  where
    median3 a b c = (a `min'` b) `max'` ((a `max'` b) `min'` c)
    max' a b =
        case a `comp` b of
          GT -> a
          EQ -> a
          LT -> b

    min' a b = 
        case a `comp` b of
          LT -> a
          EQ -> a
          GT -> b


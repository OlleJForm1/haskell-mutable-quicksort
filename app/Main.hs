module Main where

import Control.Monad.ST (runST, ST)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Control.Monad (when)
import Control.Monad.Loops (untilJust, untilM_)
import Data.STRef (newSTRef, modifySTRef, readSTRef)
import Data.Functor (($>))

quickSort :: Ord a => [a] -> [a]
quickSort xs = runST $ do
    let a = V.fromList xs
    res <- V.thaw a
    quickSort' res 0 (V.length a - 1)
    V.toList <$> V.freeze res
  where 
    quickSort' :: Ord a => VM.STVector s a -> Int -> Int -> ST s ()
    quickSort' a l h = when (l < h && l >= 0) $ do
        p <- partition a l h
        quickSort' a l p
        quickSort' a (p + 1) h

    partition :: Ord a => VM.STVector s a -> Int -> Int -> ST s Int
    partition a lo hi = do
        p  <- VM.read a lo
        l' <- newSTRef (lo - 1)
        h' <- newSTRef (hi + 1)
        untilJust $ do
            increment l' `untilM_` (p <=) <$> (a `at` l')
            decrement h' `untilM_` (p >=) <$> (a `at` h')
            l'' <- readSTRef l'
            h'' <- readSTRef h'
            if l'' >= h''
              then pure $ Just h''
              else VM.swap a l'' h'' $> Nothing

    increment r = modifySTRef r (+ 1)
    decrement r = modifySTRef r (+ (-1))
    at a r = VM.read a =<< readSTRef r



main :: IO ()
main = print $ quickSort [1, 5, 3, 6, 7, 22, 4, 6, 2, 3, 44] 


module Main where

import Control.Monad.ST (runST, ST)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Control.Monad (when)
import Control.Monad.Loops (untilJust, untilM_)
import Data.STRef (newSTRef, modifySTRef, readSTRef)
import Data.Functor (($>))
import Data.Bifoldable (bimapM_)

quickSort :: Ord a => [a] -> [a]
quickSort xs = runST $ do
    let a = V.fromList xs
    res <- V.thaw a
    quickSort' res
    V.toList <$> V.freeze res
  where 
    quickSort' :: Ord a => VM.STVector s a -> ST s ()
    quickSort' a = when (VM.length a > 1) 
                 $ partition a >>= bimapM_ quickSort' quickSort'

    partition :: Ord a => VM.STVector s a -> ST s (VM.STVector s a, VM.STVector s a)
    partition a = do
        p <- VM.read a 0
        l <- newSTRef (-1)
        h <- newSTRef $ VM.length a
        untilJust $ do
            increment l `untilM_` (p <=) <$> (a `at` l)
            decrement h `untilM_` (p >=) <$> (a `at` h)
            l' <- readSTRef l
            h' <- readSTRef h
            if l' < h'
              then VM.swap a l' h' $> Nothing
              else pure $ Just $ VM.splitAt (h' + 1) a

    increment r = modifySTRef r (+ 1)
    decrement r = modifySTRef r (+ (-1))
    at a r = VM.read a =<< readSTRef r



main :: IO ()
main = print $ quickSort [1, 5, 3, 6, 7, 22, 4, 6, 2, 3, 44] 


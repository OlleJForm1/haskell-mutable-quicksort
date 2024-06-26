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
        (l, h) = (0, V.length a - 1)
    res <- V.thaw a
    quickSort' res l h
    V.toList <$> V.freeze res
  where 
    quickSort' :: Ord a => VM.STVector s a -> Int -> Int -> ST s ()
    quickSort' a l h = when (l < h && l >= 0) $ do
        p <- partition a l h
        quickSort' a l p
        quickSort' a (p + 1) h

    partition :: Ord a => VM.STVector s a -> Int -> Int -> ST s Int
    partition a l h = do
      p  <- VM.read a l
      l' <- newSTRef (l - 1)
      h' <- newSTRef (h + 1)
      untilJust $ do
        modifySTRef l' (+   1 ) `untilM_` (>= p) <$> (VM.read a =<< readSTRef l')
        modifySTRef h' (+ (-1)) `untilM_` (<= p) <$> (VM.read a =<< readSTRef h')
        l'' <- readSTRef l'
        h'' <- readSTRef h'
        if l'' >= h''
           then pure $ Just h''
           else VM.swap a l'' h'' $> Nothing


main :: IO ()
main = print $ quickSort [1, 5, 3, 6, 7, 22, 4, 6, 2, 3, 44] 


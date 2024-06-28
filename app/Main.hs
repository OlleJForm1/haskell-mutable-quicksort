module Main where

import Control.Monad.ST (ST)
import qualified Data.Vector.Mutable as VM
import Control.Monad (when, (>=>))
import Control.Monad.Loops (untilJust, untilM_)
import Data.STRef (newSTRef, modifySTRef, readSTRef)
import Data.Functor (($>))
import Lib (withSTVector, bothM_, bothM)
import Control.Monad.Fix (fix)
import Data.Function ((&))

quickSort :: Ord a => [a] -> [a]
quickSort = withSTVector $ fix $ \rec v ->
    when (VM.length v > 1) $ do
        partition v >>= bothM_ rec
  where 
    partition :: Ord a => VM.STVector s a -> ST s (VM.STVector s a, VM.STVector s a)
    partition a = do
        p <- VM.read a 0
        (l, h) <- newSTRef `bothM` (-1, VM.length a)
        untilJust $ do
            increment l `untilM_` (p <=) <$> (a `at` l)
            decrement h `untilM_` (p >=) <$> (a `at` h)
            (l', h') <- readSTRef `bothM` (l, h)
            if l' < h'
              then VM.swap a l' h' $> Nothing
              else VM.splitAt (h' + 1) a & Just & pure

    increment = (`modifySTRef` (+   1))
    decrement = (`modifySTRef` (+ (-1)))
    at a = readSTRef >=> VM.read a

main :: IO ()
main = print $ quickSort ([1, 5, 3, 6, 7, 22, 4, 6, 2, 3, 44] :: [Int])


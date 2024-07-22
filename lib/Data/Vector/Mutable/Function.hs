{-# LANGUAGE RankNTypes #-}

module Data.Vector.Mutable.Function where

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Control.Monad.ST (ST, runST)

withSTVector :: (forall s. VM.STVector s a -> ST s ()) -> [a] -> [a]
withSTVector f xs = runST $ do 
    mutXs <- V.thaw $ V.fromList xs
    f mutXs 
    V.toList <$> V.freeze mutXs


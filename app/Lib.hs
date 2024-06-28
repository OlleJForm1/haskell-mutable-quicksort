{-# LANGUAGE RankNTypes #-}
module Lib where
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Control.Monad.ST (ST, runST)
import Data.Bitraversable (bimapM)
import Data.Bifoldable (bimapM_)

withSTVector :: (forall s. VM.STVector s a -> ST s ()) -> [a] -> [a]
withSTVector f xs = runST $ do 
    mutXs <- V.thaw $ V.fromList xs
    f mutXs 
    V.toList <$> V.freeze mutXs

bothM :: Monad m => (a -> m b) -> (a, a) -> m (b, b)
bothM f = bimapM f f

bothM_ :: Monad m => (a -> m ()) -> (a, a) -> m ()
bothM_ f = bimapM_ f f


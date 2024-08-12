module Control.Applicative.Bitraversable where

import           Data.Bitraversable (Bitraversable, bitraverse)
import           Data.Functor       (void)

bothA :: (Applicative f, Bitraversable t) => (a -> f b) -> t a a -> f (t b b)
bothA f = bitraverse f f

bothA_ :: (Applicative f, Bitraversable t) => (a -> f ()) -> t a a -> f ()
bothA_ f = void . bitraverse f f


module Control.Applicative.Tuple
  ( allT3A
  )
  where

import           Control.Monad (ap)

allT3A :: Applicative f => (a -> f b) -> (a, a, a) -> f (b, b, b)
allT3A f (a, b, c) = (,,) <$> f a <*> f b <*> f c


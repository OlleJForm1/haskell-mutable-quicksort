module Control.Applicative.Tuple
  ( allT3
  )
  where

import Control.Monad (ap)

allT3 :: Applicative f => (a -> f b) -> (a, a, a) -> f (b, b, b)
allT3 f (a, b, c) = (,,) <$> f a <*> f b <*> f c


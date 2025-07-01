{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE UnicodeSyntax #-}

module Data.Function.Loops
  ( loopM
  )
  where

loopM :: Monad m
      => (∀ b. m b -> (a -> m b) -> m b)
      -> m a
loopM f = f (loopM f) (pure)


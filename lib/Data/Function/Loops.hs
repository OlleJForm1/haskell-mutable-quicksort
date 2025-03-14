{-# LANGUAGE LambdaCase #-}

module Data.Function.Loops
  ( loopM
  , LoopResult
  )
  where

data LoopResult a = Continue | Done a

loopM :: Monad m
      => (LoopResult a
          -> (a -> m (LoopResult a))
          -> m (LoopResult a))
      -> m a
loopM f = f Continue (pure . Done) >>= \case
    Continue -> loopM f
    Done a -> pure a


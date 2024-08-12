module Data.Function.Recursive (recursive) where

import           Data.Function (fix)

recursive :: ((a -> b) -> a -> b) -> a -> b
recursive f x = fix f x


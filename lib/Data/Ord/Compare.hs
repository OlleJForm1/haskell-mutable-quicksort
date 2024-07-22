module Data.Ord.Compare where

lte :: Ordering -> Bool
lte GT = False
lte _  = True

gte :: Ordering -> Bool
gte LT = False
gte _  = True


module Data.Ord.Compare where

lessOrEqualOn :: a -> (a -> a -> Ordering) -> a -> Bool
lessOrEqualOn a c b = lte $ c a b

greaterOrEqualOn :: a -> (a -> a -> Ordering) -> a -> Bool
greaterOrEqualOn a c b = gte $ c a b

lte :: Ordering -> Bool
lte GT = False
lte _  = True

gte :: Ordering -> Bool
gte LT = False
gte _  = True


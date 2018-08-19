module Tree where

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

justFrom :: Maybe a -> a 
justFrom (Just x) = x

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing m = False








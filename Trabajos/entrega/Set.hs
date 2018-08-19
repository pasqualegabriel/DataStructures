module Set(Set, emptySet, add, belongs, isEmptySet, setToList, sizeSet, unionSet) where

data Set a = MkSet [a] Int  --deriving (Show,Eq)
{- Inv. Rep. 
+ El numero debe ser igual al largo de la Set
-}

emptySet :: Set a  -- O(1)
emptySet = MkSet [] 0

add  :: Eq a => a -> Set a -> Set a -- O(n)
add x (MkSet xs n) = if elem x xs 
                        then MkSet xs n
						else MkSet (x:xs) (n+1)

belongs :: Eq a => a -> Set a -> Bool -- O(n)
belongs x (MkSet xs n) = elem x xs
			
isEmptySet :: Set a -> Bool -- O(1)
isEmptySet (MkSet xs n) = n == 0

sizeSet :: Set a -> Int
sizeSet (MkSet xs n) = n

setToList :: Set a -> [a] -- O(1)
setToList (MkSet xs _) = xs

unionSet :: Eq a => Set a -> Set a -> Set a
unionSet (MkSet xs n1) (MkSet ys n2) = MkSet (juntar xs ys) (n1+n2)

juntar :: Eq a => [a] -> [a] -> [a]
juntar [] ys     = ys
juntar (x:xs) ys = juntar xs (agregarSiHaceFalta x ys)

agregarSiHaceFalta :: Eq a => a -> [a] -> [a]
agregarSiHaceFalta e xs = if pertenece e xs then xs else e:xs 

pertenece :: Eq a => a -> [a] -> Bool
pertenece e [] = False
pertenece e (x:xs) = (e == x) || pertenece e xs

cn :: Set Int
cn = MkSet [1,2,3,4,5] 5

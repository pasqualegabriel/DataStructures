module Set(Set, empty, add, belongs, remove, size, union) where

data Set a = MkSet [a] Int  --deriving (Show,Eq)
{- Inv. Rep. 
+ El numero debe ser igual al largo de la Set
-}

empty :: Set a  -- O(1)
empty = MkSet [] 0

add  :: Eq a => a -> Set a -> Set a -- O(n)
add x (MkSet xs n) = if elem x xs 
                        then MkSet xs n
						else MkSet (x:xs) (n+1)

belongs :: Eq a => a -> Set a -> Bool -- O(n)
belongs x (MkSet xs n) = elem x xs
			
isEmpty :: Set a -> Bool -- O(1)
isEmpty (MkSet xs n) = n == 0

size :: Set a -> Int
size (MkSet xs n) = n

setToList :: Set a -> [a] -- O(1)
setToList (MkSet xs _) = xs

union :: Eq a => Set a -> Set a -> Set a
union (MkSet xs n1) (MkSet ys n2) = MkSet (juntar xs ys) (juntarS xs ys n2)

juntar :: Eq a => [a] -> [a] -> [a]
juntar [] ys     = ys
juntar (x:xs) ys = juntar xs (agregarSiHaceFalta x ys)

agregarSiHaceFalta :: Eq a => a -> [a] -> [a]
agregarSiHaceFalta e xs = if pertenece e xs then xs else e:xs 

juntarS :: Eq a => [a] -> [a] -> Int -> Int
juntarS [] ys     n = n
juntarS (x:xs) ys n = juntarS xs ys (agregarSiHaceFaltaS x ys n)

agregarSiHaceFaltaS :: Eq a => a -> [a] -> Int -> Int
agregarSiHaceFaltaS e xs n = if pertenece e xs then n else 1+n 

pertenece :: Eq a => a -> [a] -> Bool
pertenece e [] = False
pertenece e (x:xs) = (e == x) || pertenece e xs

cn :: Set Int
cn = MkSet [1,2,3,4,5] 5

cn2 :: Set Int
cn2 = MkSet [5,6,7,8] 4

remove :: Eq a => a -> Set a -> Set a 
remove x (MkSet xs n) = if elem x xs 
                        then MkSet (sacarSet x xs) (n-1)
                        else MkSet xs n 

sacarSet :: Eq a => a -> [a] -> [a]
sacarSet x   []   = []  
sacarSet x (y:ys) = if x==y then (sacarSet x ys) else y:(sacarSet x ys)						



--module SetTree(Set, emptySet, add, belongs, isEmptySet, setToList, sizeSet, unionSet) where

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

data Set a = MkSet (Tree a) Int  

-- Invariante de representacion: 
-- El numero debe ser igual a la cantidad de raizes del arbol
-- No hay raizes repetidas
-- El Arbol es BST 

emptySet :: Set a  -- O(1)
emptySet = MkSet EmptyT 0

add  :: Ord a => a -> Set a -> Set a      -- O(log(n))
add x (MkSet t n) = MkSet (agregarS x t) (belongsSize x t n)

-- Dado un elemento y un arbol, agrega ese elemento al arbol
agregarS :: Ord a => a -> Tree a -> Tree a      -- O(log(n))
agregarS x      EmptyT       = NodeT x EmptyT EmptyT
agregarS x t@(NodeT r t1 t2) = if x==r 
	                           then t  
	                           else if x<r 
	                                then NodeT r (agregarS x t1) t2
	                                else NodeT r t1 (agregarS x t2)

-- Dado un elemento, un arbol y un numero n, devuleve n si el elemento pertenece al arbol,
-- y (n+1) si no pertenece 
belongsSize :: Ord a => a -> Tree a -> Int -> Int  -- O(log(n))
belongsSize x t n = if perteneceS x t then n else (n+1)            

-- Devuelve True si el elemento esta en el set
belongs :: Ord a => a -> Set a -> Bool   -- O(log(n))
belongs x (MkSet t n) = perteneceS x t 

-- Devuelve True si el elemento esta en el arbol
perteneceS :: Ord a => a -> Tree a -> Bool   -- O(log(n))
perteneceS x      EmptyT     = False 
perteneceS x (NodeT r t1 t2) = if x==r 
	                           then True
	                           else if x<r 
	                           	    then perteneceS x t1
	                           	    else perteneceS x t2
			
isEmptySet :: Set a -> Bool    -- O(1)
isEmptySet (MkSet t n) = n == 0

sizeSet :: Set a -> Int    -- O(1)
sizeSet (MkSet t n) = n

---------------------
setToList :: Set a -> [a] -- O(n)
setToList (MkSet t _) = listInOrder t 

-- Dado un arbol devuelve una lista que representa el resultado de recorrerlo en modo in-order.
-- Nota: En el modo in-order primero se procesan los elementos del hijo izquierdo, luego la raiz
-- y luego los elementos del hijo derecho.
listInOrder :: Tree a -> [a]   -- O(n)
listInOrder EmptyT          = [] 
listInOrder (NodeT a a1 a2) = listInOrder a1 ++ [a] ++ listInOrder a2

--unionSet :: Eq a => Set a -> Set a -> Set a
--unionSet (MkSet xs n1) (MkSet ys n2) = MkSet (juntar xs ys) (n1+n2)

--juntar :: Eq a => [a] -> [a] -> [a]
--juntar [] ys     = ys
--juntar (x:xs) ys = juntar xs (agregarSiHaceFalta x ys)

--agregarSiHaceFalta :: Eq a => a -> [a] -> [a]
--agregarSiHaceFalta e xs = if pertenece e xs then xs else e:xs 

set :: Set Int
set = MkSet arbol 27

arbol :: Tree Int 
arbol = NodeT 20 (NodeT 7 (NodeT 4 (NodeT 2 (NodeT 1 EmptyT EmptyT) (NodeT 3 EmptyT EmptyT)) 
	   (NodeT 6 (NodeT 5 EmptyT EmptyT) EmptyT)) (NodeT 12 (NodeT 8 EmptyT (NodeT 10 EmptyT EmptyT)) 
	   (NodeT 14 (NodeT 13 EmptyT EmptyT) (NodeT 17 EmptyT EmptyT)))) (NodeT 30 (NodeT 23 (NodeT 21 EmptyT 
	   (NodeT 22 EmptyT EmptyT)) (NodeT 27 (NodeT 25 EmptyT EmptyT) (NodeT 29 EmptyT EmptyT))) 
	   (NodeT 35 (NodeT 33 (NodeT 31 EmptyT EmptyT) EmptyT) (NodeT 40 (NodeT 37 EmptyT  EmptyT) 
	   (NodeT 45 EmptyT EmptyT))))

--removeS :: Ord a => a -> Set a -> Set a 
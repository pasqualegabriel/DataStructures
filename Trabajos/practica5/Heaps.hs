module Heaps where

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) 

data Dir = Izq | Der 

data Heap a = MkHeap (Tree a) [Dir]
-- Dado NodeT x t1 t2
-- x es menor que los elementos de t1 y t2
-- t1 y t2 son Heap
-- El arbol es completo y se completa de izq a der

emptyH :: Heap a
emptyH = MkHeap EmptyT []

isEmptyH :: Heap a -> Bool
isEmptyH (MkHeap EmptyT _) = True
isEmptyH (MkHeap t _)      = False   

insertH :: Ord a => a -> Heap a -> Heap a
insertH e (MkHeap t dirs) = MkHeap (insertarEnTree e t (reverse dirs)) (nextPos dirs) 

nextPos :: [Dir] -> [Dir]
nextPos   []      = [Izq]
nextPos (Izq:dir) = Der:dir
nextPos (Der:dir) = Izq:(nextPos dir)

insertarEnTree :: Ord a => a -> Tree a -> [Dir] -> Tree a 
insertarEnTree e    EmptyT          []    = NodeT e EmptyT EmptyT 
insertarEnTree e (NodeT x t1 t2) (Der:ds) = flotaEnDer x t1 (insertarEnTree e t2 ds)
insertarEnTree e (NodeT x t1 t2) (Izq:ds) = floraEnIzq x (insertarEnTree e t1 ds) t2

flotaEnDer :: Ord a => a -> Tree a -> Tree a -> Tree a 
flotaEnDer x t1 t2@(NodeT r t1' t2') = if r<x
                                       then NodeT r t1 (NodeT x t1' t2')
                                       else NodeT x t1 t2 

floraEnIzq :: Ord a => a -> Tree a -> Tree a -> Tree a 
floraEnIzq x t1@(NodeT r t1' t2') t2 = if r<x
                                       then NodeT r (NodeT x t1' t2') t2
                                       else NodeT x t1 t2 

findMin :: Ord a => Heap a -> a -- Parcial en emptyH
findMin (MkHeap t _) = raiz t 

deleteMin :: Ord a => Heap a -> Heap a -- Parcial en emptyH
deleteMin (MkHeap t dirs) = MkHeap (deleteTreeMin t (reverse (prevPos dirs))) (prevPos dirs)

prevPos :: [Dir] -> [Dir]
prevPos   [Izq]   = []
prevPos   [Der]   = [Izq]
prevPos (Der:dir) = Izq:dir
prevPos (Izq:dir) = Der:(prevPos dir)

deleteTreeMin :: Ord a => Tree a -> [Dir] -> Tree a 
deleteTreeMin t ds= let (last, tree) = splitLast t ds 
                    in if isEmptyT tree
                  	   then EmptyT
                  	   else decantar last (tIzq tree) (tDer tree)

fromJust :: Maybe a -> a 
fromJust (Just x) = x 

tIzq :: Tree a -> Tree a
tIzq (NodeT _ t1 _) = t1       

tDer :: Tree a -> Tree a
tDer (NodeT _ _ t2) = t2    

isEmptyT :: Tree a -> Bool 
isEmptyT EmptyT = True    
isEmptyT t = False       	 

splitLast :: Ord a => Tree a -> [Dir] -> (a, Tree a)
splitLast t d = (sLast t d, sLTree t d) 
--splitLast arbol [Izq,Der]

sLast :: Ord a => Tree a -> [Dir] -> a
sLast (NodeT x t1 t2) [] = x
sLast (NodeT x t1 EmptyT) _ = raiz t1
sLast (NodeT x t1 t2) (Izq:ds) = sLast t1 ds
sLast (NodeT x t1 t2) (Der:ds) = sLast t2 ds

sLTree :: Ord a => Tree a -> [Dir] -> Tree a
sLTree (NodeT x t1 t2) [] = EmptyT
sLTree (NodeT x t1 EmptyT) _ = NodeT x EmptyT EmptyT
sLTree (NodeT x t1 t2) (Izq:ds) = NodeT x (sLTree t1 ds) t2
sLTree (NodeT x t1 t2) (Der:ds) = NodeT x t1 (sLTree t2 ds)

--splitAt1 [Izq,Der] arbol
splitAt1 :: Ord a => [Dir] -> Tree a -> (a, Tree a)
splitAt1 [] (NodeT r EmptyT EmptyT) = (r, EmptyT)
splitAt1 (Izq:ds) (NodeT r t1 t2) = let (x,t)=(splitAt1 ds t1)
                                   in (x, (NodeT r t t2))
splitAt1 (Der:ds) (NodeT r t1 t2) = let (x,t)=(splitAt1 ds t2)
                                   in (x, (NodeT r t1 t))

raiz :: Tree a -> a
raiz (NodeT x _ _) = x

decantar :: Ord a => a -> Tree a -> Tree a -> Tree a 
decantar x     EmptyT          EmptyT      = NodeT x EmptyT EmptyT
decantar x (NodeT r t1 t2)    EmptyT      = if r<x
                                             then NodeT r (NodeT x EmptyT EmptyT) EmptyT
                                             else NodeT x (NodeT r t1 t2) EmptyT 
decantar x a1@(NodeT xi t1 t2) a2@(NodeT xd ti td) = if xi<x
	                                                 then NodeT xi (decantar x t1 a2) a2
	                                                 else if xd<x
	                                                 	  then NodeT xd t1 (decantar x t2 a2)
	                                                 	  else NodeT x a1 a2


splitMin :: Ord a => Heap a -> (a,Heap a) -- Parcial en emptyH 
splitMin h = (findMin h, deleteMin h)

h :: Heap Int 
h = insertH 4 (insertH 1 (insertH 12 (insertH 8 (insertH 7 emptyH))))

arbol :: Tree Int 
arbol = (NodeT 1 (NodeT 4 (NodeT 8 EmptyT EmptyT) (NodeT 7 EmptyT EmptyT)) (NodeT 12 EmptyT EmptyT))

































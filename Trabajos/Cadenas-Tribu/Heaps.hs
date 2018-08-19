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

flotaEnDer :: Ord a => a -> Tree a -> Tree a -> Tree a 
flotaEnDer x t1@(NodeT r t1' t2') t2 = if r<x
                                       then NodeT r (NodeT x t1' t2') t2
                                       else NodeT x t1 t2 

--findMin :: Ord a => Heap a -> a -- Parcial en emptyH

deleteMin :: Ord a => Heap a -> Heap a -- Parcial en emptyH
deleteMin (MkHeap t dirs) = MkHeap (deleteTreeMin t) (prevPos dirs)

deleteTreeMin :: Ord a => Tree a -> Tree a 
deleteTreeMin t = let (last, tree) = splitLast t 
                  in if isEmptyT tree
                  	 then EmptyT
                  	 else decantar last (tIzq tree) (tDer tree)

fromJust :: Maybe a -> a 
fromJust (Just x) = x 

tIzq :: Tree a -> a 
tIzq (NodeT _ t1 _) = t1       

tDer :: Tree a -> a 
tDer (NodeT _ _ t2) = t2             	 

splitLast :: Ord a => Tree a -> (a, Tree a)



decantar :: Ord a => a -> Tree a -> Tree a -> Tree a 
decantar x     EmptyT          EmptyT      = NodeT x EmptyT EmptyT
decantar x (NodeT r t1 t2)     EmptyT      = if x'<x
                                             then NodeT x' 
                                             else 
decantar x (NodeT x' t1' t2') (NodeT x'' t1'' t2'') =  


--splitMin :: Ord a => Heap a -> (a,Heap a) -- Parcial en emptyH 



































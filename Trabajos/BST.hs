
data Tree a = EmptyT | NodeT a (Tree a) (Tree a) 

-- Invariante de representacion:
-- Todos los elementos de t1 son menores que x
-- Todos los elementos de t2 son mayores que x
-- t1 y t2 son BST

--data Map k v = MkMap (Tree (k,v))

--Dado un BST inserta un elemento en el arbol.
insertBST :: Ord a => a -> Tree a -> Tree a
insertBST x      EmptyT       = NodeT x EmptyT EmptyT
insertBST x t@(NodeT r t1 t2) = if x<r then NodeT r (insertBST x t1) t2 else 
	                            if x==r then t else NodeT r  t1 (insertBST x t2)                                

--Dado un BST dice si el elemento pertenece o no al arbol.
--Precondicion: El arbol es BST
perteneceBST :: Ord a => a -> Tree a -> Bool
perteneceBST e     EmptyT      = False
perteneceBST e (NodeT r t1 t2) = if e==r then True else 
                                 if e<r then perteneceBST e t1 else perteneceBST e t2 

--Dado un BST devuelve un par con el mınimo elemento y el arbol sin el mismo.
--Precondicion: El elemto debe estar en el arbol
splitMinBST :: Ord a => Tree a -> (a, Tree a)
splitMinBST (NodeT r EmptyT EmptyT) = (r, EmptyT)
splitMinBST    t@(NodeT r t1 t2)    = (minT t, eliminarMin t)

minT :: Ord a => Tree a -> a 
minT (NodeT r EmptyT _) = r
minT (NodeT r t1 _) = minT t1

eliminarMin :: Tree a -> Tree a
eliminarMin (NodeT x EmptyT t2) = t2
eliminarMin (NodeT x t1 t2) = NodeT x (eliminarMin t1) t2

--Dado un BST devuelve un par con el maximo elemento y el arbol sin el mismo.
splitMaxBST :: Ord a => Tree a -> (a, Tree a)
splitMaxBST (NodeT r EmptyT EmptyT) = (r, EmptyT)
splitMaxBST    t@(NodeT r t1 t2)    = (maxT t, eliminarMax t)

maxT :: Ord a => Tree a -> a 
maxT (NodeT r _ EmptyT) = r
maxT (NodeT r _ t2) = maxT t2

eliminarMax :: Ord a => Tree a -> Tree a 
eliminarMax (NodeT r t1 EmptyT) = t1
eliminarMax  t@(NodeT r t1 t2)  = NodeT r t1 (eliminarMax t2) 

--Pasa a lista de forma ordenada
toList :: Ord a => Tree a -> [a]
toList EmptyT          = [] 
toList (NodeT r t1 t2) = (toList t1)++[r]++(toList t2)

--Dado un BST y un elemento, devuelve el maximo elemento que sea menor al elemento dado.
-- Precondicion: Debe haber por lo menos un elemento que cumpla con el proposito en el arbol
elMaximoMenorA :: Ord a => a -> Tree a -> a
elMaximoMenorA m (NodeT r EmptyT EmptyT) = r
elMaximoMenorA m (NodeT r t1 EmptyT) = if (maxT t1<m) && r<m
	                                   then r 
	                                   else elMaximoMenorA m t1
elMaximoMenorA m (NodeT r EmptyT t2)   = if (minT t2>=m) && r<m
	                                     then r 
	                                     else elMaximoMenorA m t2
elMaximoMenorA m     (NodeT r t1 t2)     = let minimoT = (minT t2>=m) 
	                                       in if (maxT t1<m) && minimoT && r<m
	                                          then r 
	                                          else if minimoT
	                                       	       then elMaximoMenorA m t1
	                                               else elMaximoMenorA m t2

raiz :: Tree a -> a 
raiz (NodeT r t1 t2) = r	                                               

--Dado un BST y un elemento, devuelve el minimo elemento que sea mayor al elemento dado
--elMinimoMayorA :: Ord a => a -> Tree a -> a

arbol :: Tree Int 
arbol = NodeT 20 (NodeT 7 (NodeT 4 (NodeT 2 (NodeT 1 EmptyT EmptyT) (NodeT 3 EmptyT EmptyT)) 
	(NodeT 6 (NodeT 5 EmptyT EmptyT) EmptyT)) (NodeT 12 (NodeT 8 EmptyT (NodeT 10 EmptyT EmptyT)) 
	(NodeT 14 (NodeT 13 EmptyT EmptyT) (NodeT 17 EmptyT EmptyT)))) (NodeT 30 (NodeT 23 (NodeT 21 EmptyT 
		(NodeT 22 EmptyT EmptyT)) (NodeT 27 (NodeT 25 EmptyT EmptyT) (NodeT 29 EmptyT EmptyT))) 
	(NodeT 35 (NodeT 33 (NodeT 31 EmptyT EmptyT) EmptyT) (NodeT 40 (NodeT 37 EmptyT  EmptyT) 
		(NodeT 45 EmptyT EmptyT))))

--                                            20
--                           ---------------------------------------------       
--                           7                                           30
--                   -------------------                      ------------------------
--                   4                 12                    23                      35
--             -----------       -------------         ---------------          --------------
--             2         6       8           14        21           27          33           40
--          ------    -----    ------     -------   --------     -------     --------      ------- 
--          1    3    5   E    E    10    13   17   E     22     25    29    31     E      37    45
 

arbol2 :: Tree Int 
arbol2 = NodeT 20 (NodeT 7 (NodeT 4 (NodeT 2 (NodeT 1 EmptyT EmptyT) (NodeT 3 EmptyT EmptyT)) 
	(NodeT 6 (NodeT 5 EmptyT EmptyT) EmptyT)) (NodeT 12 (NodeT 8 EmptyT (NodeT 10 EmptyT EmptyT)) 
	EmptyT)) (NodeT 30 (NodeT 23 (NodeT 21 EmptyT (NodeT 22 EmptyT EmptyT)) (NodeT 27 
	(NodeT 25 EmptyT EmptyT) (NodeT 29 EmptyT EmptyT)))	(NodeT 35 EmptyT 
	(NodeT 40 (NodeT 37 EmptyT  EmptyT)	(NodeT 45 EmptyT EmptyT))))

--                                            20
--                           ---------------------------------------------       
--                           7                                           30
--                   -------------------                      ------------------------
--                   4                 12                    23                      35
--             -----------       -------------         ---------------          --------------
--             2         6       8           E        21           27           E            40
--          ------    -----    ------               --------     -------                   ------- 
--          1    3    5   E    E    10              E     22     25    29                 37     45


















































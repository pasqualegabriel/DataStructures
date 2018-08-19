module BSTPasquale where

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) 

-- Invariante de representacion:
-- Todos los elementos de t1 son menores que x
-- Todos los elementos de t2 son mayores que x
-- t1 y t2 son BST

--Dado un BST y un elemento, devuelve el maximo elemento que sea menor al elemento dado.
-- Precondicion: Debe haber por lo menos un elemento en el arbol que cumpla con el proposito
elMaximoMenorA :: Ord a => a -> Tree a -> a
elMaximoMenorA m (NodeT r EmptyT EmptyT) = if r<m then r else error "No hay ningun elemento"
elMaximoMenorA m (NodeT r t1 EmptyT) = if r<m
	                                   then r 
	                                   else elMaximoMenorA m t1
elMaximoMenorA m (NodeT r EmptyT t2)   = if (minT t2>=m) && r<m
	                                     then r 
	                                     else elMaximoMenorA m t2
elMaximoMenorA m     (NodeT r t1 t2)     = let minimoT = (minT t2>=m) 
	                                       in if minimoT && r<m
	                                          then r 
	                                          else if minimoT
	                                       	       then elMaximoMenorA m t1
	                                               else elMaximoMenorA m t2

elMaximoMenorA3 :: Ord a => a -> Tree a -> a
elMaximoMenorA3 n EmptyT              = error "no existe"
elMaximoMenorA3 n (NodeT x t1 t2) = if x<n 
                                       then elMaximoMenorAconCandidato n x t2 
                                       else elMaximoMenorA3 n t1

elMaximoMenorAconCandidato :: Ord a => a -> a -> Tree a -> a
elMaximoMenorAconCandidato n c EmptyT          = c
elMaximoMenorAconCandidato n c (NodeT x t1 t2) = if x<n 
	                                             then elMaximoMenorAconCandidato n x t2
	                                             else elMaximoMenorAconCandidato n c t1

elMaximoMenorA2 :: Ord a => a -> Tree a -> a
elMaximoMenorA2 n t = let m=elMaximoMenorMaybe n t 
                      in if isNothing m 
                         then error "no existe" 
                         else fromJust m 

elMaximoMenorMaybe :: Ord a => a -> Tree a -> Maybe a
elMaximoMenorMaybe n EmptyT          = Nothing
elMaximoMenorMaybe n (NodeT x t1 t2) = if x<n 
	                                   then elMayorCandidato x (elMaximoMenorMaybe n t2)
	                                   else elMaximoMenorMaybe n t1 

elMayorCandidato :: Ord a => a -> Maybe a -> Maybe a
elMayorCandidato x Nothing  = Just x
elMayorCandidato x (Just n) = Just n 

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing   j     = False

fromJust :: Maybe a -> a 
fromJust (Just x) = x 

-- Dado un BST, devuelve al elemento mayor
-- Precondicion: El BST no es vacio
maxT :: Ord a => Tree a -> a 
maxT (NodeT r _ EmptyT) = r
maxT (NodeT r _ t2) = maxT t2

-- Dado un BST, devuelve al elemento menor
-- Precondicion: El BST no es vacio
minT :: Ord a => Tree a -> a 
minT (NodeT r EmptyT _) = r
minT (NodeT r t1 _) = minT t1

--Dado un BST y un elemento, devuelve el minimo elemento que sea mayor al elemento dado
-- Precondicion: Debe haber por lo menos un elemento en el arbol que cumpla con el proposito
elMinimoMayorA :: Ord a => a -> Tree a -> a	                                               
elMinimoMayorA m (NodeT r EmptyT EmptyT) = if r>m then r else error "No hay ningun elemento"
elMinimoMayorA m (NodeT r t1 EmptyT) = if (maxT t1<=m) && r>m
	                                   then r 
	                                   else elMinimoMayorA m t1
elMinimoMayorA m (NodeT r EmptyT t2)   = if (minT t2>m) && r>m
	                                     then r 
	                                     else elMinimoMayorA m t2
elMinimoMayorA m     (NodeT r t1 t2)     = if (maxT t1<=m) && (minT t2>m) && r>m
	                                       then r 
	                                       else if (maxT t1>=m) 
	                                            then elMinimoMayorA m t1
	                                            else elMinimoMayorA m t2

arbol :: Tree Int 
arbol = NodeT 20 (NodeT 7 (NodeT 4 (NodeT 2 (NodeT 1 EmptyT EmptyT) (NodeT 3 EmptyT EmptyT)) 
	(NodeT 6 (NodeT 5 EmptyT EmptyT) EmptyT)) (NodeT 12 (NodeT 8 EmptyT (NodeT 10 EmptyT EmptyT)) 
	(NodeT 14 (NodeT 13 EmptyT EmptyT) (NodeT 17 EmptyT EmptyT)))) (NodeT 30 (NodeT 23 (NodeT 21 EmptyT 
		(NodeT 22 EmptyT EmptyT)) (NodeT 27 (NodeT 25 EmptyT EmptyT) (NodeT 29 EmptyT EmptyT))) 
	(NodeT 35 (NodeT 33 (NodeT 31 EmptyT EmptyT) EmptyT) (NodeT 40 (NodeT 37 EmptyT  EmptyT) 
		(NodeT 45 EmptyT EmptyT))))

--                                                20
--                            ---------------------------------------------       
--                            7                                          30
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

--                                                20
--                            --------------------------------------------       
--                            7                                          30
--                   -------------------                      ------------------------
--                   4                 12                    23                      35
--             -----------       -------------         ---------------          --------------
--             2         6       8           E        21           27           E            40
--          ------    -----    ------               --------     -------                   ------- 
--          1    3    5   E    E    10              E     22     25    29                 37     45
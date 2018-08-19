module MapTreePasquale(Map, emptyM, assocM, lookupM, removeM, domM) where

import Set

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) 

data Map k v = MkMap (Tree (k, v))

-- Invariante de representacion:
-- Las claves no están repetidas
-- El arbol es BST

emptyM :: Map k v    -- O(1)
emptyM = MkMap EmptyT

assocM :: Ord k => Map k v -> k -> v -> Map k v   -- O(log(n))
assocM (MkMap t) k v = MkMap (agregarBST t k v) 

-- Dado un arbol (kr,vr), una clave k y un valor v, agrega (k,v) al arbol
agregarBST :: Ord k =>  Tree (k,v) -> k -> v -> Tree (k,v)     -- O(log(n))
agregarBST     EmptyT            ki vi = NodeT (ki,vi) EmptyT EmptyT
agregarBST (NodeT (kr,vr) t1 t2) ki vi = if ki==kr
	                                     then NodeT (kr,vi) t1 t2
	                                     else if ki<kr
	                                     	  then NodeT (kr,vr) (agregarBST t1 ki vi) t2
	                                     	  else NodeT (kr,vr) t1 (agregarBST t2 ki vi) 

lookupM :: Ord k => Map k v -> k -> Maybe v    --O(log(n))
lookupM (MkMap t) k = buscarBST t k 

-- Dado un arbol (k,v) y una clave, devuelve un Maybe v con el valor de la clave 
--  o Nothing si no esta la clave en el arbol
buscarBST :: Ord k => Tree (k,v) -> k -> Maybe v   --O(log(n))
buscarBST    EmptyT             k  = Nothing 
buscarBST (NodeT (kr,vr) t1 t2) k = if k==kr 
	                                then Just vr 
	                                else if k<kr
	                                	 then buscarBST t1 k 
	                                	 else buscarBST t2 k 

removeM :: Ord k => Map k v -> k -> Map k v    --O(log(n))
removeM (MkMap t) k = MkMap (borrarBST t k) 

-- Dado un arbol (k,v) y una clave, elimina la tupla de esa clave del arbol
borrarBST :: Ord k => Tree (k,v) -> k -> Tree (k,v)      --O(log(n))
borrarBST        EmptyT         k = EmptyT
borrarBST (NodeT (kr,vr) t1 t2) k = if k==kr
	                                then rearmarBST t1 t2
	                                else if k<kr 
	                                	 then NodeT (kr,vr) (borrarBST t1 k) t2
	                                	 else NodeT (kr,vr) t1 (borrarBST t2 k) 

-- Dado dos arboles (k,v), devuelve un arvol con todas las raizes de ambos arboles
rearmarBST :: Eq k => Tree (k,v) -> Tree (k,v) -> Tree (k,v)     --O(log(n))
rearmarBST EmptyT          EmptyT           = EmptyT
rearmarBST EmptyT            t2             = t2
rearmarBST  t1             EmptyT           = t1
rearmarBST  t1    (NodeT (kr,vr) EmptyT t2) = (NodeT (kr,vr) t1 t2)
rearmarBST  t1    (NodeT (kr,vr) ti td)     = (NodeT (kr,vr) (rearmarBST t1 ti) td) 

domM :: Eq k => Map k v -> Set k      -- O(claves)
domM (MkMap t) = claves t 

-- Dado un arbol (k,v), devuelve un Set con sus claves
claves :: Eq k => Tree (k,v) -> Set k     -- O(unionSet)
claves       EmptyT          = emptySet 
claves (NodeT (kr,vr) t1 t2) = unionSet (unionSet (claves t1) (claves t2)) (add kr emptySet)

map :: Map Int Int
map = MkMap (NodeT (4,40) (NodeT (3,30) (NodeT (1,10) EmptyT (NodeT (2,20) EmptyT EmptyT)) EmptyT) 
	(NodeT (6,60) (NodeT (5,50) EmptyT EmptyT) (NodeT (8,80) (NodeT (7,70) EmptyT EmptyT) (NodeT (9,90) EmptyT EmptyT))))



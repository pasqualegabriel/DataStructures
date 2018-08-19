import Tree
-- EmptyT, NodeT
import ConjuntoSufijos
--ConjuntoSufijos, cs1, cs2, vacio, terminanCon, agregarSufijos
import Set
--Set, empty, add, belongs, remove, size, union

conjuntoSufijos :: Tree ConjuntoSufijos -> Set String
conjuntoSufijos      EmptyT        = empty
conjuntoSufijos (NodeT conS t1 t2) = union (terminanCon [] conS) (union (conjuntoSufijos t1) (conjuntoSufijos t2))

arbol :: Tree ConjuntoSufijos
arbol = NodeT cs1 EmptyT (NodeT cs2 EmptyT EmptyT)

-- Precondicion: la lista no es vacio y hay al menos un string en el conjuntoSufijo
sufijoMayor :: [String] -> ConjuntoSufijos -> String
sufijoMayor  [x]   conS = x 
sufijoMayor (x:xs) conS = elSufijoMayor x conS (sufijoMayor xs conS)

elSufijoMayor :: String -> ConjuntoSufijos -> String -> String
elSufijoMayor s1 conS s2 = if (tamanioCS s1 conS)>(tamanioCS s2 conS)
	                       then s1 
	                       else s2

tamanioCS :: String -> ConjuntoSufijos -> Int
tamanioCS s conS = size (terminanCon s conS)

ls :: [String]
ls = ["boca","ball","dragon","contratapa"]

























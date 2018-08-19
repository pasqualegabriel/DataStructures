import Set
{- 
data Set a

emptySet :: Set a
Crea un conjunto vacio

add :: Eq a => a -> Set a -> Set a
Dados un elemento y un conjunto, agrega el elemento al conjunto.

belongs :: Eq a => a -> Set a -> Bool
Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.

union :: Eq a => Set a -> Set a -> Set a
Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos conjuntos.

intersection :: Eq a => Set a -> Set a -> Set a
Dados dos conjuntos devuelve el conjunto de elementos que ambos conjuntos tienen en comun.

setToList :: Set a -> [a]
Dado un conjunto devuelve una lista con todos los elementos del conjunto.
-}

-- Quita todos los elementos repetidos de la lista dada utilizando un conjunto como estructura auxiliar.
--sinRepetidos :: [a] -> [a]

-- Dados una cola y un conjunto, devuelve una cola con todos los elementos que pertenecen al conjunto.
--losQuePertenecen :: Eq a => Queue a -> Set a -> Queue a

-- Dada una stack de conjuntos devuelve un conjunto con la union de todos los conjuntos de la stack.
--unirTodos :: Stack (Set a) -> Set a

-- Dados dos arboles en los que sus elementos no se repiten devuelve un conjunto con los
-- elementos que ambos arboles tienen en comun. Utilizar el tipo abstracto conjunto como
-- estructura auxiliar para calcular el conjunto de elementos en comun de ambos arboles.
--intersectTree :: Eq a => Tree a -> Tree a -> Set a


















import Conjunto
{- 
Un Conjunto es un tipo abstracto de datos que consta de las siguientes
operaciones:

vacioC :: Conjunto a
Crea un conjunto vacío.

agregarC :: Eq a => a -> Conjunto a -> Conjunto a
Dados un elemento y un conjunto, agrega el elemento al conjunto.

perteneceC :: Eq a => a -> Conjunto a -> Bool
Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.

cantidadC :: Eq a => Conjunto a -> Int
Devuelve la cantidad de elementos distintos de un conjunto

borrarC :: Eq a => a -> Conjunto a -> Conjunto a
Dados un elemento y un conjunto, retorna el conjunto sin a

unionC :: Eq a => Conjunto a -> Conjunto a -> Conjunto a
Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos conjuntos.

listaC :: Eq a => Conjunto a -> [a]
Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.
-}
-- Dados una lista y un conjunto, devuelve una lista con todos los elementos que pertenecen al conjunto.
losQuePertenecen :: Eq a => [a] -> Conjunto a -> [a]
losQuePertenecen [] c     = []
losQuePertenecen (x:xs) c = if perteneceC x c 
	                        then x:losQuePertenecen xs c
	                        else losQuePertenecen xs c 

-- Quita todos los elementos repetidos de la lista dada utilizando un conjunto como estructura auxiliar.
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos xs = listaC (listaAConjunto xs)

listaAConjunto :: Eq a => [a] -> Conjunto a
listaAConjunto []     = vacioC 
listaAConjunto (x:xs) = unionC (agregarC x vacioC) (listaAConjunto xs)

data Tree a = Empty | Nodo a (Tree a) (Tree a)

-- Dado un arbol de conjuntos devuelve un conjunto con la union de todos los conjuntos del arbol.
unirTodos :: Eq a => Tree (Conjunto a) -> Conjunto a
unirTodos Empty          = vacioC
unirTodos (Nodo a t1 t2) = unionC (unionC a (unirTodos t1)) (unirTodos t2)

-- Dados dos árboles devuelve un conjunto con los elementos que ambos árboles tienen en común.
interseccionArbol :: Eq a => Tree a -> Tree a -> Conjunto a
interseccionArbol t1 t2 = unionC (arbolAConjunto t1 vacioC) (arbolAConjunto t2 vacioC)

arbolAConjunto :: Eq a => Tree a -> Conjunto a -> Conjunto a
arbolAConjunto (Empty) c        = c
arbolAConjunto (Nodo a t1 t2) c = unionC (unionC (agregarC a c) (arbolAConjunto t1 c)) (arbolAConjunto t2 c)

c1 = Mkc [1,2,3,4]

a = (Nodo 1 (Nodo 2 Empty Empty) (Nodo 3 Empty (Nodo 4 Empty Empty)))

b = (Nodo 5 (Nodo 3 Empty Empty) (Nodo 7 Empty (Nodo 4 Empty Empty)))































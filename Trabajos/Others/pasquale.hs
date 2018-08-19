
--Pasquale Gabriel

--Estructuras de datos

data Tree a = EmptyT
            | NodeT a (Tree a) (Tree a)

-- Ejercicio 18
-- Dados un numero n y un árbol devuelve una lista con los NodeTs de nivel n.
-- Nota: El primer nivel de un árbol (su raíz) es 0.
-- Precondicion: n>=0 y n <= al último nivel del árbol
levelN :: Int -> Tree a -> [a]
levelN 0 EmptyT          = []
levelN 0 (NodeT a a1 a2) = [a]
levelN n (NodeT a a1 a2) = levelN (n-1) a1 ++ levelN (n-1) a2

-- Ejercicio 19
-- Dado un árbol devuelve una lista de listas en la que cada elemento representa un nivel de dicho árbol.
listPerLevel :: Tree a -> [[a]]
listPerLevel tree = nivelN (heightT tree) tree  

-- Dada la cantidad de niveles del árbol y el árbol, devuelve una lista de listas en la que cada 
-- elemento representa un nivel de dicho árbol.
nivelN :: Int -> Tree a -> [[a]]
nivelN 0 tree = []
nivelN n tree = nivelN (n-1) tree ++ [levelN (n-1) tree]

-- Ejercicio 20
-- Dado un árbol devuelve su ancho, que es la cantidad de nodos del nivel con mayor cantidad de nodos.
widthT :: Tree a -> Int
widthT t = mayorNs (anchoEnNiveles (heightT t) t)

-- Dado un número n (la altura del árbol) y un árbol, devuelve una lista con el numero de ancho de cada nivel. 
anchoEnNiveles :: Int -> Tree a -> [Int]
anchoEnNiveles 0 t = [length (levelN 0 t)]
anchoEnNiveles n t = [length (levelN n t)] ++ anchoEnNiveles (n-1) t

-- Dada una lista de números, devuelve el número mayor de la lista.
mayorNs :: [Int] -> Int
mayorNs [n]    = n 
mayorNs (n:ns) = max n (mayorNs ns)

-- Dado un arbol devuelve su altura.
heightT :: Tree a -> Int
heightT EmptyT          = 0 
heightT (NodeT a a1 a2) = 1 + max (heightT a1) (heightT a2)
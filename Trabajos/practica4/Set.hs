module Set where

data Set a = Cons [a]
-- Invariante de representacion: la lista de (Cons xs) no tiene repetidos.

-- Crea un conjunto vacio
emptySet :: Set a
emptySet = Cons []

-- Dados un elemento y un conjunto, agrega el elemento al conjunto.
add :: Eq a => a -> Set a -> Set a
add x (Cons xs) = Cons (x:xs)

-- Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
belongs :: Eq a => a -> Set a -> Bool
belongs x (Cons ys) = pertenece x ys

pertenece :: Eq a => a -> [a] -> Bool
pertenece e [] = False
pertenece e (x:xs) = (e == x) || pertenece e xs

-- Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos conjuntos.
union :: Eq a => Set a -> Set a -> Set a
union (Cons xs) (Cons ys) = Cons (juntar xs ys)

juntar :: Eq a => [a] -> [a] -> [a]
juntar [] ys     = ys
juntar (x:xs) ys = juntar xs (agregarSiHaceFalta x ys)

agregarSiHaceFalta :: Eq a => a -> [a] -> [a]
agregarSiHaceFalta e xs = if pertenece e xs then xs else e:xs 

-- Dados dos conjuntos devuelve el conjunto de elementos que ambos conjuntos tienen en comun.
intersection :: Eq a => Set a -> Set a -> Set a
intersection (Cons xs) (Cons ys) = Cons (enAmbos xs ys [])

enAmbos :: Eq a => [a] -> [a] -> [a] -> [a]
enAmbos [] ys zs     = []
enAmbos xs [] zs     = []
enAmbos (x:xs) ys zs = agregaSiNoEsta x ys (enAmbos xs ys zs)

agregaSiNoEsta :: Eq a => a -> [a] -> [a] -> [a]
agregaSiNoEsta e xs ys = if pertenece e xs then e:ys else ys

-- Dado un conjunto devuelve una lista con todos los elementos del conjunto.
setToList :: Set a -> [a]
setToList (Cons xs) = xs











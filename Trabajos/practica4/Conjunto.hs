module Conjunto where

data Conjunto a = Mkc [a]

-- Invariante de representacion: Los elementos de la lista no tiene repetidos

-- Crea un conjunto vacío.
vacioC :: Conjunto a
vacioC = Mkc []

-- Dados un elemento y un conjunto, agrega el elemento al conjunto.
agregarC :: Eq a => a -> Conjunto a -> Conjunto a
agregarC e (Mkc xs) = Mkc (e:xs)

-- Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
perteneceC :: Eq a => a -> Conjunto a -> Bool
perteneceC e (Mkc [])     = False 
perteneceC e (Mkc (x:xs)) = (e==x) || (perteneceC e (Mkc xs)) 

-- Devuelve la cantidad de elementos distintos de un conjunto
cantidadC :: Eq a => Conjunto a -> Int
cantidadC (Mkc xs) = length xs

-- Dados un elemento y un conjunto, retorna el conjunto sin a
borrarC :: Eq a => a -> Conjunto a -> Conjunto a
borrarC e (Mkc [])     = Mkc []
borrarC e (Mkc (x:xs)) = if e==x then borrarC e (Mkc xs) else agregarC x (borrarC e (Mkc xs))

-- Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos conjuntos.
unionC :: Eq a => Conjunto a -> Conjunto a -> Conjunto a
unionC (Mkc xs) (Mkc ys) = Mkc (juntar xs ys)

juntar :: Eq a => [a] -> [a] -> [a]
juntar [] ys     = ys
juntar (x:xs) ys = juntar xs (agregarSiHaceFalta x ys)

agregarSiHaceFalta :: Eq a => a -> [a] -> [a]
agregarSiHaceFalta e xs = if pertenece e xs then xs else e:xs 

pertenece :: Eq a => a -> [a] -> Bool
pertenece e [] = False
pertenece e (x:xs) = (e == x) || pertenece e xs

-- Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.
listaC :: Eq a => Conjunto a -> [a] 
listaC (Mkc xs) = xs

-- Devuelve el máximo elemento en un conjunto
-- Precondicion: not (null xs)
maximoC :: Ord a => Conjunto a -> a
maximoC (Mkc [x])    = x
maximoC (Mkc (x:xs)) = mayor x (maximoC (Mkc xs))

mayor :: Ord a => a -> a -> a
mayor x y = if x>y then x else y 

cn :: Conjunto Int
cn = Mkc [1,2,3,4,2]











module Catalogo(Catalogo, crearCatalogo, agregarPelicula, generosCatalogo, peliculasDe, mejoresN) where

import Pelicula
--Pelicula, Genero, sW1, sW2, sW3, sW4, sW5, sW6, sW7
--comienzaCon :: String -> Pelicula -> Bool
--generos :: Pelicula -> [Genero]
--titulo :: Pelicula -> String
--puntaje :: Pelicula -> Int
import Heap
--emptyH :: Heap a
--isEmptyH :: Heap a -> Bool
--insertH :: Ord a => a -> Heap a -> Heap a
--findMin :: Ord a => Heap a -> a -- Parcial en emptyH
--deleteMin :: Ord a => Heap a -> Heap a -- Parcial en emptyH
--splitMin :: Ord a => Heap a -> (a,Heap a) -- Parcial en emptyH
import Set 
--empty :: Set a  -- O(1)
--add  :: Eq a => a -> Set a -> Set a -- O(n)
--belongs :: Eq a => a -> Set a -> Bool -- O(n)
--isEmpty :: Set a -> Bool -- O(1)
--size :: Set a -> Int
--setToList :: Set a -> [a] -- O(1)
--union :: Eq a => Set a -> Set a -> Set a
--remove :: Eq a => a -> Set a -> Set a 
import Map				
--emptyM :: Map k v
--assocM :: Eq k => Map k v -> k -> v -> Map k v
--lookupM :: Eq k => Map k v -> k -> Maybe v
--removeM :: Eq k => Map k v -> k -> Map k v
--domM :: Eq k => Map k v -> Set k

data Catalogo = MkC (Map Genero [Pelicula]) (Heap Pelicula)
-- La estructura Catalogo tiene un map donde la key son generos con la lista de
--  peliculas de ese genero como valor 
--  y un Heap de Peliculas ordenadar por puntaje
-- Una pelicula con puntaje 1 es mejor que 2 

-- Se pide:
-- Los invariantes de represenatacion de Catalogo
-- y implementar la estructura catalogo y usuarioCatalogo

-- A
-- Crear un catalogo vacio
crearCatalogo :: Catalogo

-- B
-- Dada una pelicula y un catalogo, agregar la pelicula al catalogo
agregarPelicula :: Pelicula -> Catalogo -> Catalogo

-- C
-- Dado un catalogo devuelve una lista con todos los catalogos
generosCatalogo :: Catalogo -> [Genero]

-- D
-- Dado un genero y un catalogo, devuelve una lista con todas las peliculas de ese genero
peliculasDe :: Genero -> Catalogo -> [Pelicula]

-- E
-- Dado un numero n y un catalogo, devuelve una lista con las mejores n peliculas
mejoresN :: Int -> Catalogo -> [Pelicula]













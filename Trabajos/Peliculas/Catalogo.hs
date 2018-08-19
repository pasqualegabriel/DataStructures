module Catalogo(Catalogo, crearCatalogo, agregarPelicula, generosCatalogo, peliculasDe, mejoresN, starWars) where

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

-- Inv de Rep:
--    + Si una pelicula esta en Map tambien esta en Heap
--    + Si una pelicula esta en Heap tambien esta en Map
--    + Cada pelicula debe estar en la lista de todos sus generos

-- A
-- Crear un catalogo vacio
crearCatalogo :: Catalogo
crearCatalogo = MkC emptyM emptyH

-- B
-- Dada una pelicula y un catalogo, agregar la pelicula al catalogo
agregarPelicula :: Pelicula -> Catalogo -> Catalogo
agregarPelicula p (MkC map heap) = MkC (addPelicula map p (generos p)) (insertH p heap)

addPelicula :: Map Genero [Pelicula] -> Pelicula -> [Genero] -> Map Genero [Pelicula]
addPelicula map p  [g]   = assocM map g (agregarP p (lookupM map g))
addPelicula map p (g:gs) = assocM (addPelicula map p gs) g (agregarP p (lookupM map g))

agregarP :: Pelicula -> Maybe [Pelicula] -> [Pelicula]
agregarP p  Nothing  = [p]
agregarP p (Just ps) = p:ps

starWars :: Catalogo
starWars = agregarPelicula sW4 (agregarPelicula sW7 (agregarPelicula sW1 (agregarPelicula sW3 crearCatalogo)))

-- C
-- Dado un catalogo devuelve una lista con todos los catalogos
generosCatalogo :: Catalogo -> [Genero]
generosCatalogo (MkC map heap) = setToList (domM map)

-- D
-- Dado un genero y un catalogo, devuelve una lista con todas las peliculas de ese genero
peliculasDe :: Genero -> Catalogo -> [Pelicula]
peliculasDe g (MkC map heap) = maybePelicula (lookupM map g)

maybePelicula :: Maybe [Pelicula] -> [Pelicula]
maybePelicula  Nothing  = []
maybePelicula (Just ps) = ps

-- E
-- Dado un numero n y un catalogo, devuelve una lista con las mejores n peliculas
mejoresN :: Int -> Catalogo -> [Pelicula]
mejoresN n (MkC map heap) = mejoresPeliculas n heap

mejoresPeliculas :: Int -> Heap Pelicula -> [Pelicula]
mejoresPeliculas 0 heap = []
mejoresPeliculas n heap = findMin heap : (mejoresPeliculas (n-1) (deleteMin heap))












import Heap
--emptyH :: Heap a
--isEmptyH :: Heap a -> Bool
--insertH :: Ord a => a -> Heap a -> Heap a
--findMin :: Ord a => Heap a -> a -- Parcial en emptyH
--deleteMin :: Ord a => Heap a -> Heap a -- Parcial en emptyH
--splitMin :: Ord a => Heap a -> (a,Heap a) -- Parcial en emptyH
import Pelicula
--Pelicula, Genero, sW1, sW2, sW3, sW4, sW5, sW6, sW7
--comienzaCon :: String -> Pelicula -> Bool
--generos :: Pelicula -> [Genero]
--titulo :: Pelicula -> String
--puntaje :: Pelicula -> Int
import Set 
--empty :: Set a  -- O(1)
--add  :: Eq a => a -> Set a -> Set a -- O(n)
--belongs :: Eq a => a -> Set a -> Bool -- O(n)
--isEmpty :: Set a -> Bool -- O(1)
--size :: Set a -> Int
--setToList :: Set a -> [a] -- O(1)
--union :: Eq a => Set a -> Set a -> Set a
--remove :: Eq a => a -> Set a -> Set a
--listToSet :: [a] -> Set a  
import Catalogo
--crearCatalogo :: Catalogo
--agregarPelicula :: Pelicula -> Catalogo -> Catalogo
--generosCatalogo :: Catalogo -> [Genero]
--peliculasDe :: Genero -> Catalogo -> [Pelicula]
--mejoresN :: Int -> Catalogo -> [Pelicula]

-- A
-- Dado un catalogo, devuelve cada genero con la cantidad de peliculas de ese genero en el catalogo
cantidadesDe :: Catalogo -> [(Genero, Int)]

-- B 
-- Dado un genero y un catalogo, devuelve un Heap con las peliculas de ese genero
ordenadoPorPuntaje :: Genero -> Catalogo -> Heap Pelicula

-- C 
-- Dado un catalogo, devuelve el genero con la mayor cantidad de peliculas con puntaje 1
maxGeneroConPuntajeUno :: Catalogo -> Genero

-- D
-- Dado un string s y un catalogo, devuelve un heap con las peliculas que comienzan con s
-- La estructura pelicula ya cuenta con comienzaCon :: String -> Pelicula -> Bool
peliculasQueComienzanCon :: String -> Catalogo -> Heap Pelicula









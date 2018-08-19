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
cantidadesDe c = cantidad (generosCatalogo c) c 
 
cantidad :: [Genero] -> Catalogo -> [(Genero, Int)]
cantidad  []    c = []
cantidad (g:gs) c = cantPeliculas c g : cantidad gs c 

cantPeliculas :: Catalogo -> Genero -> (Genero, Int)
cantPeliculas c g = (g, length (peliculasDe g c))

-- B 
-- Dado un genero y un catalogo, devuelve un Heap con las peliculas de ese genero
ordenadoPorPuntaje :: Genero -> Catalogo -> Heap Pelicula
ordenadoPorPuntaje g c = peliculasOrdenadas (peliculasDe g c) 

peliculasOrdenadas :: [Pelicula] -> Heap Pelicula
peliculasOrdenadas   []   = emptyH
peliculasOrdenadas (p:ps) = insertH p (peliculasOrdenadas ps)

-- C 
-- Dado un catalogo, devuelve el genero con la mayor cantidad de peliculas con puntaje 1
maxGeneroConPuntajeUno :: Catalogo -> Genero
maxGeneroConPuntajeUno c = maxGenero (generosCatalogo c) c 

maxGenero :: [Genero] -> Catalogo -> Genero
maxGenero  [g]   c = g
maxGenero (g:gs) c = mayorGenero g (maxGenero gs c) c

mayorGenero :: Genero -> Genero -> Catalogo -> Genero
mayorGenero g1 g2 c = if cantPelisUno g1 c > cantPelisUno g2 c 
	                  then g1
	                  else g2

cantPelisUno :: Genero -> Catalogo -> Int 
cantPelisUno g c = pelisUno (peliculasDe g c)

pelisUno :: [Pelicula] -> Int 
pelisUno   []   = 0 
pelisUno (p:ps) = if ((puntaje p)==1) then 1+pelisUno ps else pelisUno ps 

-- D
-- Dado un string s y un catalogo, devuelve un heap con las peliculas que comienzan con s
-- La estructura pelicula ya cuenta con comienzaCon :: String -> Pelicula -> Bool
peliculasQueComienzanCon :: String -> Catalogo -> Heap Pelicula
peliculasQueComienzanCon s c = pelisComienzanCon s (generosCatalogo c) c

pelisComienzanCon :: String -> [Genero] -> Catalogo -> Heap Pelicula
pelisComienzanCon s gs c = agregarSiComienzaCon s (listaDePeliculas gs c)
--agregarSiComienzaCon s (setToList (listToSet (listaDePeliculas gs c))) 

agregarSiComienzaCon :: String -> [Pelicula] -> Heap Pelicula
agregarSiComienzaCon s   []   = emptyH
agregarSiComienzaCon s (p:ps) = if comienzaCon s p 
	                            then insertH p (agregarSiComienzaCon s ps)
	                            else agregarSiComienzaCon s ps 

listaDePeliculas :: [Genero] -> Catalogo -> [Pelicula]
listaDePeliculas   []   c = []
listaDePeliculas (g:gs) c = agregaSiNoEsta (peliculasDe g c) (listaDePeliculas gs c)
--(peliculasDe g c)++(listaDePeliculas gs c)

agregaSiNoEsta :: [Pelicula] -> [Pelicula] -> [Pelicula]
agregaSiNoEsta   []   ps = ps
agregaSiNoEsta (x:xs) ps = if elem x ps then agregaSiNoEsta xs ps else x:agregaSiNoEsta xs ps 



























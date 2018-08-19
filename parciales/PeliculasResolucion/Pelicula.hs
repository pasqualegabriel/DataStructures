module Pelicula(Pelicula, Genero, comienzaCon, generos, titulo, puntaje, sW1, sW2, sW3, sW4, sW5, sW6, sW7) where

data Pelicula = MKP [Genero] Titulo Puntaje

type Titulo = String
type Genero = String
type Puntaje = Int
-- El puntaje 1 es mejor que 2

instance Eq Pelicula where
    p1 == p2 = (puntaje p1)==(puntaje p2) && (titulo p1)==(titulo p2)

instance Ord Pelicula where
    p1 > p2 = (puntaje p1) > (puntaje p2)
    p1 < p2 = (puntaje p1) < (puntaje p2)
    p1 >= p2 = (puntaje p1) >= (puntaje p2)
    p1 <= p2 = (puntaje p1) <= (puntaje p2) 

comienzaCon :: String -> Pelicula -> Bool
comienzaCon s (MKP gs n p) = comienza s n 

comienza :: String -> String -> Bool
comienza   []      y   = True
comienza (x:xs) (y:ys) = (x==y) && (comienza xs ys)

generos :: Pelicula -> [Genero]
generos (MKP gs n p) = gs

titulo :: Pelicula -> String
titulo (MKP gs n p) = n

puntaje :: Pelicula -> Int
puntaje (MKP gs n p) = p

sW1 :: Pelicula
sW1 = MKP ["Accion", "Ficcion"] "star wars 1" 1

sW2 :: Pelicula
sW2 = MKP ["Drama", "Ficcion"] "sw2" 2

sW3 :: Pelicula
sW3 = MKP ["Accion", "Comedia"] "starwars3" 3

sW4 :: Pelicula
sW4 = MKP ["Accion"] "star wars 4" 4 

sW5 :: Pelicula
sW5 = MKP ["Ficcion"] "star wars 5" 5

sW6 :: Pelicula
sW6 = MKP ["Drama", "Comedia", "Ficcion"] "star wars 6" 6

sW7 :: Pelicula
sW7 = MKP ["Accion", "Comedia"] "theforceawakens" 1


























module Tablero where

import Map
import Set

{-
emptyM  :: Map k v
assocM  :: Eq k => Map k v -> k -> v -> Map k v
lookupM :: Eq k => Map k v -> k -> Maybe v
deleteM :: Eq k => Map k v -> k -> Map k v
domM    :: Map k v -> Set k
-}	
{-
Invariante de representacion:
- Para la Coordenada actual, siempre hay un valor en el Map, no puede retornar Nothing
- Las Coordenadas no pueden ser negativas
- La cantidad de bolitas no pueden ser negativas
- El Map no puede ser vacio
-}

data Dir = Norte | Sur | Este | Oeste

data Color = Azul | Negro | Rojo | Verde

data Coordenada = MkCoord Int Int -- x, y

data Celda = MkCelda Int Int Int Int -- Azul, Negro, Rojo, Verde

data Tablero = MkTablero (Map Coordenada Celda) Coordenada

instance Eq Coordenada where
    c1 == c2 = (xCoord c1 == xCoord c2) && (yCoord c1 == yCoord c2)
--    c1 == c2 = False

instance Ord Coordenada where
    c1 > c2 = (yCoord c1 > yCoord c2) || ((yCoord c1==yCoord c2) && (xCoord c1 > xCoord c2))
    c1 < c2 = (yCoord c1 < yCoord c2) || ((yCoord c1==yCoord c2) && (xCoord c1 < xCoord c2))

xCoord :: Coordenada -> Int
xCoord (MkCoord x y) = x 

yCoord :: Coordenada -> Int
yCoord (MkCoord x y) = y 

poner :: Color -> Tablero -> Tablero
poner col (MkTablero mapCeldas actual) = MkTablero (ponerEnMap col mapCeldas actual) actual

fromJust :: Maybe a -> a
fromJust (Just x) = x

ponerEnMap :: Color -> Map Coordenada Celda -> Coordenada -> Map Coordenada Celda
ponerEnMap col mapCeldas coord =  assocM mapCeldas coord (ponerEnCelda (buscarCelda mapCeldas coord) col)

-- La celda exite en map
buscarCelda :: Map Coordenada Celda -> Coordenada -> Celda
buscarCelda mapCeldas coord = fromJust (lookupM mapCeldas coord)

ponerEnCelda :: Celda -> Color -> Celda
ponerEnCelda (MkCelda a n r v) Azul  = MkCelda (a+1) n r v
ponerEnCelda (MkCelda a n r v) Negro = MkCelda a (n+1) r v
ponerEnCelda (MkCelda a n r v) Rojo  = MkCelda a n (r+1) v
ponerEnCelda (MkCelda a n r v) Verde = MkCelda a n r (v+1)

tablero :: Tablero
tablero = MkTablero (MkM [(MkCoord 0 0,MkCelda 0 0 0 0),(MkCoord 0 1,MkCelda 0 0 0 0),
	                      (MkCoord 1 0,MkCelda 1 0 0 0),(MkCoord 1 1,MkCelda 0 0 0 0)]) (MkCoord 0 0)

sacar :: Color -> Tablero -> Tablero
sacar col (MkTablero mapCeldas actual) = MkTablero (sacarEnMap col mapCeldas actual) actual

--sacarEnMap :: Color -> Map Coordenada Celda -> Coordenada -> Map Coordenada Celda
--sacarEnMap col mapCeldas coord =  assocM mapCeldas coord (sacarEnCelda (buscarCelda mapCeldas coord) col)
sacarEnMap :: Color -> Map Coordenada Celda -> Coordenada -> Map Coordenada Celda
sacarEnMap col mapCeldas coord = let celda = buscarCelda mapCeldas coord
                                  in  if hayBolitas celda col 
                                  	  then assocM mapCeldas coord (sacarEnCelda celda col) 
                                  	  else error "BOOM"

sacarEnCelda :: Celda -> Color -> Celda
sacarEnCelda (MkCelda a n r v) Azul  = MkCelda (a-1) n r v
sacarEnCelda (MkCelda a n r v) Negro = MkCelda a (n-1) r v
sacarEnCelda (MkCelda a n r v) Rojo  = MkCelda a n (r-1) v
sacarEnCelda (MkCelda a n r v) Verde = MkCelda a n r (v-1)

hayBolitas :: Celda -> Color -> Bool
hayBolitas (MkCelda a n r v) Azul  = a>0
hayBolitas (MkCelda a n r v) Negro = n>0
hayBolitas (MkCelda a n r v) Rojo  = r>0
hayBolitas (MkCelda a n r v) Verde = v>0

puedeMover :: Dir -> Tablero -> Bool
puedeMover dir (MkTablero mapCeldas actual) = existeEnTablero (moverDir dir actual) mapCeldas
--puedeMover puedeMover dir (MkTablero mapCeldas actual) = ! isNothing (lookupM (moverDir dir actual) mapCeldas)

existeEnTablero :: Coordenada -> Map Coordenada Celda -> Bool
existeEnTablero coord mapCeldas = belongs coord (domM mapCeldas)

moverDir :: Dir -> Coordenada -> Coordenada
moverDir Norte (MkCoord n1 n2) = MkCoord n1 (n2+1)
moverDir Este  (MkCoord n1 n2) = MkCoord (n1+1) n2
moverDir Sur   (MkCoord n1 n2) = MkCoord n1 (n2-1)
moverDir Oeste (MkCoord n1 n2) = MkCoord (n1-1) n2

mover :: Dir -> Tablero -> Tablero 
mover dir tablero = if puedeMover dir tablero 
	                then actualizarPosicionActual dir tablero
	                else error "BOOM"

actualizarPosicionActual :: Dir -> Tablero -> Tablero
actualizarPosicionActual dir (MkTablero mapCeldas actual) = MkTablero mapCeldas (moverDir dir actual)

nroBolitas :: Color -> Tablero -> Int
nroBolitas color (MkTablero mapCeldas actual) = cantidadC color (buscarCelda mapCeldas actual)
 
cantidadC :: Color -> Celda -> Int
cantidadC Azul  (MkCelda a n r v) = a
cantidadC Negro (MkCelda a n r v) = n
cantidadC Rojo  (MkCelda a n r v) = r
cantidadC Verde (MkCelda a n r v) = v

posicionActual :: Tablero -> Coordenada
posicionActual (MkTablero mapCeldas actual) = actual

nuevoTablero :: Int -> Int -> Tablero
nuevoTablero x y = MkTablero (agregarCyF x y emptyM) (MkCoord 0 0)

agregarCyF :: Int -> Int -> Map Coordenada Celda -> Map Coordenada Celda
agregarCyF 0 y m = agregarColumnas 0 y m 
agregarCyF x y m = agregarColumnas x y (agregarCyF (x-1) y m)

agregarColumnas :: Int -> Int -> Map Coordenada Celda -> Map Coordenada Celda
agregarColumnas x 0 m = creaColumnas x 0 m
agregarColumnas x y m = creaColumnas x y (agregarColumnas x (y-1) m) 

creaColumnas :: Int -> Int -> Map Coordenada Celda -> Map Coordenada Celda
creaColumnas x y m = assocM m (MkCoord x y) (MkCelda 0 0 0 0)

nuevoTableroN :: Int -> Int -> Tablero
nuevoTableroN x y = MkTablero (crearT x y) (MkCoord 0 0)

crearT :: Int -> Int -> Map Coordenada Celda
crearT 0 y = crearfila 0 y
crearT x y = unirMaps (crearfila x y) (crearT (x-1) y)

crearfila :: Int -> Int -> Map Coordenada Celda
crearfila x 0 = assocM emptyM (MkCoord x 0) (MkCelda 0 0 0 0)
crearfila x y = assocM (crearfila x (y-1)) (MkCoord x y) (MkCelda 0 0 0 0)

unirMaps :: Eq a => Map a b -> Map a b -> Map a b
unirMaps a b = unirLMap (setToList (domM a)) a b 

unirLMap :: Eq a => [a] -> Map a b -> Map a b -> Map a b
unirLMap   []   mx m = m
unirLMap (x:xs) mx m = assocM (unirLMap xs mx m) x (fromJust (lookupM mx x))

-- GOBSTONES

irAlExtremo :: Dir -> Tablero -> Tablero
irAlExtremo dir t = if puedeMover dir t 
                    then irAlExtremo dir (mover dir t)
                    else t



















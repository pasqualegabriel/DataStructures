module ConjuntoSufijos(ConjuntoSufijos, cs1, cs2, vacio, terminanCon, agregarSufijos, borrarSufijos) where

import Map 
-- Map, emptyM, assocM, lookupM, removeM, domM
import Set
-- Set, empty, add, belongs, remove, size, union
import Tree

data ConjuntoSufijos = CP (Map String (Set String))

vacio :: ConjuntoSufijos 
vacio = CP emptyM 

terminanCon :: String -> ConjuntoSufijos -> Set String
terminanCon k (CP map) = setMaybe (lookupM map k)

setMaybe :: Maybe (Set String) -> Set String
setMaybe Nothing  = empty 
setMaybe (Just x) = x

agregarSufijos :: String -> ConjuntoSufijos -> ConjuntoSufijos
agregarSufijos p (CP map) = CP (agregarPalabra p p map)

agregarPalabra :: String -> String -> Map String (Set String) -> Map String (Set String)
agregarPalabra   []   p map = assocM map [] (addPalabraSet p (lookupM map [])) 
agregarPalabra (x:xs) p map = assocM (agregarPalabra xs p map) (x:xs) (addPalabraSet p (lookupM map (x:xs)))
--agregarPalabra xs p (assocM map (x:xs) (addPalabraSet p (lookupM map (x:xs))))
--assocM (agregarPalabra xs p map) (x:xs) (addPalabraSet p (lookupM map (x:xs)))

addPalabraSet :: String -> Maybe (Set String) -> Set String
addPalabraSet p  Nothing   = add p empty
addPalabraSet p (Just set) = add p set 

cs1 :: ConjuntoSufijos
cs1 = agregarSufijos "contratapa" (agregarSufijos "tapa" (agregarSufijos "cola" (agregarSufijos "hola" vacio)))

cs2 :: ConjuntoSufijos
cs2 = agregarSufijos "star" (agregarSufijos "wars" (agregarSufijos "dragon" (agregarSufijos "ball" vacio)))

borrarSufijos :: String -> ConjuntoSufijos -> ConjuntoSufijos
borrarSufijos p (CP conS) = CP (borrarPalabra p p conS) 

borrarPalabra :: String -> String -> Map String (Set String) -> Map String (Set String)
borrarPalabra    []    p map = actualizarOBorrar [] p map 
borrarPalabra s@(x:xs) p map = actualizarOBorrar s p (borrarPalabra xs p map)

deletePalabraSet :: String -> Maybe (Set String) -> Set String
deletePalabraSet s  Nothing   = empty
deletePalabraSet s (Just set) = remove s set 

actualizarOBorrar :: String -> String -> Map String (Set String) -> Map String (Set String)
actualizarOBorrar s p map = let dps = (deletePalabraSet p (lookupM map s))
	                        in if size dps==0
                               then removeM map s
                               else assocM map s dps























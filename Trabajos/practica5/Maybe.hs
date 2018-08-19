module Maybe where 

import Map 
import Set

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

-- parcial en nothing
fromJust :: Maybe a -> a
fromJust (Just x) = x


minL :: Ord a => [a] -> Maybe a
minL []     = Nothing
minL (x:xs) = minMaybe (Just x) (minL xs)

minT :: Ord a => Tree a -> Maybe a
minT EmptyT          = Nothing
minT (NodeT x t1 t2) = minMaybe (Just x) (minMaybe (minT t1) (minT t2))

minMaybe :: Ord a => Maybe a -> Maybe a -> Maybe a
minMaybe Nothing Nothing  = Nothing
minMaybe Nothing (Just y) = Just y
minMaybe (Just x) Nothing = Just x
minMaybe (Just x) (Just y)= Just (min x y)

-- Dada una lista quita su ultimo elemento.
-- parcial en lista vacia
initC :: [a] -> [a]
initC [x]    = [] 
initC (x:xs) = x:(initC xs)

-- total con maybe
initT  :: [a] -> Maybe [a]
initT []     = Nothing
initT [x]    = Just []
initT (x:xs) = juntarMaybes (Just [x]) (initT xs)

juntarMaybes :: Maybe [a] -> Maybe [a] -> Maybe [a]
juntarMaybes Nothing Nothing    = Nothing
juntarMaybes Nothing (Just ys)  = Just ys
juntarMaybes (Just xs) Nothing  = Just xs
juntarMaybes (Just xs) (Just ys)= Just (xs++ys)

initT2  :: [a] -> Maybe [a]
initT2 [] = Nothing
initT2 xs = Just (init xs)

-- Dada una lista, devuelve su ultimo elemento.
-- parcial en lista vacia
lastC :: [a] -> a
lastC [x]   = x
lastC (x:xs)= lastC xs 

--total
lastT :: [a] -> Maybe a
lastT []     = Nothing
lastT [x]    = Just x
lastT (x:xs) = lastT xs 

-- Dado un elemento y una lista devuelve la posicion de la lista en la que se encuentra dicho elemento.
-- Precondicion: El elemento debe existir en esa posicion
indiceDe :: Ord a => a -> [a] -> Int
indiceDe e (x:xs) = if e==x then 1 else 1+(indiceDe e xs) 

indiceDeT :: Ord a => a -> [a] -> Maybe Int
indiceDeT e []     = Nothing
indiceDeT e (x:xs) = if e==x then Just 1 else Just (1+(fromJust (indiceDeT e xs))) 

indiceDeT2 :: Ord a => a -> [a] -> Maybe Int
indiceDeT2 e []     = Nothing
indiceDeT2 e (x:xs) = if e==x then Just 1 else sumMaybe 1 (indiceDeT2 e xs) 

sumMaybe :: Int -> Maybe Int -> Maybe Int
sumMaybe n Nothing  = Nothing
sumMaybe n (Just x) = Just (n+x)

-- Dada una lista de pares (clave, valor) y una clave devuelve el valor asociado a la clave.
--valorParaClave :: Eq k => [(k,v)] -> k -> v
--valorParaClave (x:xs) k = if fst x==k then snd x else valorParaClave xs k

valorParaClave :: Eq k => [(k,v)] -> k -> Maybe v
valorParaClave [] key       = Nothing
valorParaClave (kv:kvs) key = if key==fst kv then Just (snd kv) else (valorParaClave kvs key)

-- Dada una lista de elementos devuelve el maximo.
maximumT :: Ord a => [a] -> Maybe a
maximumT []     = Nothing
maximumT (x:xs) = maxMaybe (Just x) (maximumT xs) 

maxMaybe :: Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing Nothing  = Nothing
maxMaybe Nothing (Just y) = Just y
maxMaybe (Just x) Nothing = Just x
maxMaybe (Just x) (Just y)= Just (max x y)

-- Dado un arbol devuelve su elemento maximo. 
maxT :: Ord a => Tree a -> Maybe a
maxT EmptyT          = Nothing
maxT (NodeT a t1 t2) = maxMaybe (Just a) (maxMaybe (maxT t1) (maxT t2))
{-
emptyM  :: Map k v
assocM  :: Eq k => Map k v -> k -> v -> Map k v
lookupM :: Eq k => Map k v -> k -> Maybe v
deleteM :: Eq k => Map k v -> k -> Map k v
domM    :: Map k v -> Set k

Dada una lista de nombres de personas y un Map que relaciona nombres con numeros de telefonos, 
devuelve una lista con los numeros de las personas de la lista o Nothing en caso de que no posea numero.
-}
pedirTelefonos :: [String] -> Map String Int -> [Maybe Int]
pedirTelefonos [] map     = []
pedirTelefonos (t:ts) map = (lookupM map t):(pedirTelefonos ts map)

-- Dado un string cuenta las ocurrencias de cada caracter utilizando un Map.
ocurrencias :: String -> Map Char Int
ocurrencias []     = emptyM
ocurrencias (c:cs) = if existeClave c (ocurrencias cs) 
                     then assocM (ocurrencias cs) c (fromJust (lookupM (ocurrencias cs) c) +1)
                     else assocM (ocurrencias cs) c 1

ocurrencias2 :: String -> Map Char Int
ocurrencias2 []     = emptyM
ocurrencias2 (c:cs) = let rec = (ocurrencias2 cs)
                     in
                     if existeClave c rec 
                     then assocM rec c (fromJust (lookupM rec c) +1)
                     else assocM rec c 1

existeClave :: Char -> Map Char Int -> Bool
existeClave c map = belongs c (domM map)                    

-- Dada una lista de elementos construye un Map que relaciona cada elemento con su posicion en la lista.
indexar :: [a] -> Map Int a
indexar xs = agClave xs 1 (indexar xs)

agClave :: [a] -> Int -> Map Int a -> Map Int a  
agClave [] n map     = emptyM  
agClave (x:xs) n map = assocM (agClave xs (n+1) map) n x

-- Devuelve la lista de valores, usar let para obtener el resultado del lookUp
-- Precondicion: Las claves de [k] existen en Map k v
obtenerClaves :: Ord k => [k] -> Map k v -> [v]
obtenerClaves   []   map = []
obtenerClaves (x:xs) map = fromJust (lookupM map x):(obtenerClaves xs map)








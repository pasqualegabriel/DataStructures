
import Map
{-
emptyM  :: Map k v
assocM  :: Eq k => Map k v -> k -> v -> Map k v
lookupM :: Eq k => Map k v -> k -> Maybe v
removeM :: Eq k => Map k v -> k -> Map k v
domM    :: Map k v -> Set k
-}

-- Invariante de representacion: Sea MkRal m n
-- Dado n = cantidad de claves de m
-- Las claves de m van de 0 a (n-1)

data Ralist a = MkRal (Map Int a) Int

ral :: Ralist Int
ral =  MkRal (MkM [(0,10),(1,20),(2,30),(3,40)]) 4

emptyRal :: Ralist a 
emptyRal = MkRal emptyM 0

addRal :: a -> Ralist a -> Ralist a
addRal e (MkRal map n) = MkRal (assocM map n e) (n+1)

-- la lista no esta vacia
removeRal :: Ralist a -> Ralist a
removeRal (MkRal map n) = MkRal (removeM map (n-1)) (n-1)

-- el indice esta en la lista
get :: Int -> Ralist a -> a 
get k (MkRal map n) = fromJust (lookupM map k)

fromJust :: Maybe a -> a
fromJust (Just x) = x

-- el indice esta en la lista
set :: Int -> a -> Ralist a -> Ralist a 
set k v (MkRal map n) = MkRal (assocM map k v) n 

sizeRal :: Ralist a -> Int
sizeRal (MkRal map n) = n

-- Como usuario

listToRalist :: [a] -> Ralist a     -- o(n^2)
listToRalist   []   = emptyRal
listToRalist (x:xs) = addRal x (listToRalist xs)

-- el elemento existe en la lista
-- Devuelve el indice del elemento a 
indexOF :: Eq a => a -> Ralist a -> Int 
indexOF e (MkRal map 1) = if e==fromJust (lookupM map 0) then 0 else error "boom"
indexOF e (MkRal map n) = if e==fromJust (lookupM map (n-1)) then n-1 else indexOF e (MkRal map (n-1)) 

indexOF2 :: Eq a => a -> Ralist a -> Int 
indexOF2 e (MkRal map n) = buscarRal n e map

buscarRal :: Eq a => Int -> a -> Map Int a -> Int 
buscarRal 0 v map = if v==fromJust (lookupM map 0) then 0 else error "boom"
buscarRal k v map = if v==fromJust (lookupM map (k-1)) then k-1 else buscarRal (k-1) v map 

-- retorna una lista de los elementos que estan en esa posicion
-- los elementos de la lista estan en Ralist a
elemsEn :: [Int] -> Ralist a -> [a]
elemsEn   []    (MkRal map n)  = []
elemsEn (x:xs) r@(MkRal map n) = (get x r):(elemsEn xs r)



























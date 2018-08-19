module Map(Map, emptyM, assocM, lookupM, removeM, domM) where

import Set

data Map k v = MkM [(k, v)]

-- Inv. Rep.
-- Las claves no están repetidas

emptyM :: Map k v
emptyM = MkM []

-- O(n)
-- porque agregarAssoc es O(n)
assocM :: Eq k => Map k v -> k -> v -> Map k v
assocM (MkM kvs) k v = MkM (agregarAssoc k v kvs) 

-- O(n)
-- porque buscarPorClave es O(n)
-- porque no repite claves
lookupM :: Eq k => Map k v -> k -> Maybe v
lookupM (MkM kvs) k = buscarPorClave k kvs

-- O(n)
-- porque borrarClave es O(n)
removeM :: Eq k => Map k v -> k -> Map k v
removeM (MkM kvs) k = MkM (borrarClave k kvs)

-- O(n)
-- porque devolverClaves es O(n)
domM :: Eq k => Map k v -> Set k
domM (MkM kvs) = devolverClaves kvs

------------------------------------------------

-- O(n)
-- porque recorre en peor caso cada clave hasta el final 
agregarAssoc :: Eq k => k -> v -> [(k, v)] -> [(k, v)]
agregarAssoc k v [] = (k, v) : []
agregarAssoc k v ((k2,v2) : kvs) =
    if k == k2
       then (k, v) : kvs
       else (k2, v2) : agregarAssoc k v kvs

-- O(n)
-- porque recorre en peor caso cada clave hasta el final 
buscarPorClave :: Eq k => k -> [(k,v)] -> Maybe v
buscarPorClave k [] = Nothing
buscarPorClave k ((k2, v) : kvs) =
    if k == k2
       then Just v
       else buscarPorClave k kvs

-- O(n)
-- porque recorre en peor caso cada clave hasta el final 
borrarClave :: Eq k => k -> [(k, v)] -> [(k, v)]
borrarClave k [] = []
borrarClave k ((k2, v) : kvs) =
    if k == k2
        then kvs
        else (k2, v) : borrarClave k kvs

-- O(n)
-- porque siempre recorre todo hasta el final
-- suponemos que add nos cuesta O(1)
devolverClaves :: Eq k => [(k, v)] -> Set k
devolverClaves [] = empty
devolverClaves ((k, _) : kvs) = 
    add k (devolverClaves kvs)


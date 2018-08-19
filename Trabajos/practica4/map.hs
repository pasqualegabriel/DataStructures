
data Map k v = Mkm [(k,v)]

emptyM :: Map k v
emptyM = Mkm []

assocM :: Ord k => k -> v -> Map k v -> Map k v 
assocM k v (Mkm kvs) = Mkm (agregarAssoc k v kvs)

agregarAssoc :: Ord k => k -> v -> [(k,v)] -> [(k,v)]
agregarAssoc k v [] = [(k,v)]
agregarAssoc k v ((k2,v2):kvs) = if k==k2 then (k,v):kvs else (k2,v2):agregarAssoc k v kvs



















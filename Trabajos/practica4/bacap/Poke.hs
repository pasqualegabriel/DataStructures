import Set
{- 
data Set a

emptySet :: Set a
Crea un conjunto vacio

add :: Eq a => a -> Set a -> Set a
Dados un elemento y un conjunto, agrega el elemento al conjunto.

belongs :: Eq a => a -> Set a -> Bool
Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.

union :: Eq a => Set a -> Set a -> Set a
Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos conjuntos.

intersection :: Eq a => Set a -> Set a -> Set a
Dados dos conjuntos devuelve el conjunto de elementos que ambos conjuntos tienen en comun.

setToList :: Set a -> [a]
Dado un conjunto devuelve una lista con todos los elementos del conjunto.
-}

data Entrenador = E String [Pokemon]

data Pokemon = Poke EspeciePok Int

data TipoPok = Fuego | Planta | Agua | Electrico

data EspeciePok = Ar | Bu | Ch | Sq

instance Eq EspeciePok where
    Ar == Ar = True
    Bu == Bu = True
    Ch == Ch = True
    Sq == Sq = True
    _  == _  = False

entrenadores :: [Entrenador]
entrenadores = [entrenador1,entrenador2,entrenador3]

entrenador1 :: Entrenador
entrenador1 = E "aa" [p1,p2,p3] 

p1 :: Pokemon
p1 = Poke Bu 2

p2 :: Pokemon
p2 = Poke Bu 1

p3 :: Pokemon
p3 = Poke Sq 3

entrenador2 :: Entrenador
entrenador2 = E "bbb" [p5,p6,p1,p2] 

entrenador3 :: Entrenador
entrenador3 = E "bbb" [p1,p6,p2,p5] 

p5 :: Pokemon
p5 = Poke Bu 1

p6 :: Pokemon
p6 = Poke Sq 3

cualesEspecies :: [Entrenador] -> [EspeciePok]
cualesEspecies es = setToList (cualesEspeciesRec es)

cualesEspeciesRec :: [Entrenador] -> Set EspeciePok
cualesEspeciesRec [] = emptySet
cualesEspeciesRec (e:es) = union (especiesDe e) (cualesEspeciesRec es)

especiesDe :: Entrenador -> Set EspeciePok
especiesDe (E n xs) = especiePok xs

especiePok :: [Pokemon] -> Set EspeciePok
especiePok []     = emptySet
especiePok (x:xs) = add (especieDe x) (especiePok xs)

especieDe :: Pokemon -> EspeciePok
especieDe (Poke e n) = e
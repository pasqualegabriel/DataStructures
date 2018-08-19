-- 1 Arboles Binarios
data Tree a = EmptyT
            | NodeT a (Tree a) (Tree a)

-- 1
--Dado un arbol binario de enteros devuelve la suma entre sus elementos.
sumarT :: Tree Int -> Int
sumarT (EmptyT)        = 0
sumarT (NodeT a a1 a2) = a + sumarT a1 + sumarT a2  

-- 2
-- Dado un arbol binario devuelve su cantidad de elementos, es decir, el tamaño del arbol.
sizeT :: Tree a -> Int
sizeT (EmptyT)        = 1
sizeT (NodeT a a1 a2) = 1 + sizeT a1 + sizeT a2
--sizeT (NodeT Este (NodeT Oeste EmptyT (NodeT Norte EmptyT EmptyT)) (NodeT Sur (NodeT Sur EmptyT EmptyT) EmptyT))

--3
--Dado un arbol de enteros devuelve un arbol con el doble de cada numero.
mapDobleT :: Tree Int -> Tree Int
mapDobleT (EmptyT)        = EmptyT 
mapDobleT (NodeT a a1 a2) = NodeT (a*2) (mapDobleT a1) (mapDobleT a2) 
--mapDobleT (NodeT 2 EmptyT (NodeT 3 (NodeT 1 EmptyT EmptyT) EmptyT))

-- 4
-- Dado un arbol de direcciones t devuelve un arbol con la direccion opuesta para cada elemento de t.
-- Nota: Utilizar el tipo Dir ya definido.
data Dir = Norte | Sur | Este | Oeste

opuesto :: Dir -> Dir
opuesto Norte = Sur
opuesto Sur   = Norte
opuesto Oeste = Este
opuesto Este  = Oeste

siguiente :: Dir -> Dir
siguiente Norte = Este
siguiente Sur = Oeste
siguiente Oeste = Norte
siguiente Este = Sur

mapOpuestoT :: Tree Dir -> Tree Dir
mapOpuestoT EmptyT          = EmptyT
mapOpuestoT (NodeT d a1 a2) = NodeT (opuesto d) (mapOpuestoT a1) (mapOpuestoT a2)
--mapOpuestoT (NodeT Este (NodeT Oeste EmptyT (NodeT Norte EmptyT EmptyT)) (NodeT Sur (NodeT Sur EmptyT EmptyT) EmptyT))

-- 5
--Dado un arbol de palabras devuelve un arbol con la longitud de cada palabra.
mapLongitudT :: Tree String -> Tree Int
mapLongitudT EmptyT          = EmptyT
mapLongitudT (NodeT s a1 a2) = NodeT (longitud s) (mapLongitudT a1) (mapLongitudT a2)
--mapLongitudT (NodeT "Hola" (NodeT "arboles" EmptyT EmptyT) (NodeT "a" EmptyT (NodeT "aa" EmptyT EmptyT)))
longitud :: [a] -> Int
longitud []     = 0
longitud (x:xs) = 1 + longitud xs

-- 6
--Dados un elemento y un arbol binario devuelve True si existe un elemento igual a ese en el arbol.
perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT e EmptyT          = False
perteneceT e (NodeT x a1 a2) = (e==x) || (perteneceT e a1) || (perteneceT e a2)
--perteneceT 1 (NodeT 2 EmptyT (NodeT 3 (NodeT 1 EmptyT EmptyT) EmptyT))

-- 7 
-- Dados un elemento e y un arbol binario devuelve la cantidad de elementos del arbol que son iguales a e.
aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT _ EmptyT          = 0
aparicionesT e (NodeT a a1 a2) = (if e==a then 1 else 0) + aparicionesT e a1 + aparicionesT e a2 
--aparicionesT 1 (NodeT 1 EmptyT (NodeT 3 (NodeT 1 EmptyT (NodeT 1 EmptyT EmptyT)) EmptyT))

-- 8 
-- Dado un arbol de personas devuelve el promedio entre las edades de todas las personas.
-- Definir las subtareas que sean necesarias para resolver esta funcion.
-- Nota: Utilizar el tipo Persona ya definido.
data Persona = MkPersona String Int

promedioEdadesT :: Tree Persona -> Int
promedioEdadesT a = promedio (listaDeRaizesDeT a)
--promedioEdadesT (NodeT (MkPersona "aa" 4) EmptyT (NodeT (MkPersona "aa" 5) 
--EmptyT (NodeT (MkPersona "aa" 6) EmptyT EmptyT)))

listaDeRaizesDeT :: Tree Persona -> [Int]
listaDeRaizesDeT EmptyT          = [] 
listaDeRaizesDeT (NodeT p a1 a2) = [edad p] ++ listaDeRaizesDeT a1 ++ listaDeRaizesDeT a2

edad :: Persona -> Int
edad (MkPersona _ e) = e

promedio :: [Int] -> Int
promedio [] = 0
promedio xs = div (sum xs) (length xs)

-- 9 
-- Dados dos arboles construye un arbol t en el que ambos arboles son hijos de t, y en la raíz
-- de t se guarda la suma de todos los elementos de los hijos de t. 
-- ¿Se utiliza recursion para definir esta funcion?
engancharYSumaEnRaiz :: Tree Int -> Tree Int -> Tree Int
engancharYSumaEnRaiz t1 t2 = NodeT (sumarT t1 + sumarT t2) t1 t2 
-- engancharYSumaEnRaiz (NodeT 1 EmptyT (NodeT 3 (NodeT 1 EmptyT (NodeT 1 EmptyT EmptyT)) EmptyT)) 
-- (NodeT 4 EmptyT (NodeT 3 (NodeT 1 EmptyT (NodeT 2 EmptyT EmptyT)) EmptyT))

-- 10 
-- Dado un arbol devuelve su cantidad de hojas.
-- Nota: una hoja (leaf en ingles) es un NodeT que no tiene hijos.
leaves :: Tree a -> Int
leaves EmptyT                  = 0
leaves (NodeT a EmptyT EmptyT) = 1
leaves (NodeT a a1 a2)         = leaves a1 + leaves a2
-- leaves (NodeT Este (NodeT Oeste EmptyT (NodeT Norte EmptyT EmptyT)) (NodeT Sur (NodeT Sur EmptyT EmptyT) EmptyT))

-- 11
-- Dado un arbol devuelve su altura.
-- Nota: la altura (height en ingles) de un arbol es la cantidad maxima de NodeTs entre la raíz
-- y alguna de sus hojas. La altura de un arbol vacío es cero y la de una hoja es 1.
heightT :: Tree a -> Int
heightT EmptyT          = 0 
heightT (NodeT a a1 a2) = 1 + max (heightT a1) (heightT a2)
--heightT (NodeT 1 EmptyT (NodeT 2 (NodeT 3 (NodeT 4 EmptyT EmptyT) EmptyT) EmptyT))

-- 12
-- Dado un arbol devuelve el numero de NodeTs que no son hojas. 
-- ¿Como podra resolverla sin utilizar recursion? Primero definirla con recursion y despues sin ella.
nodes :: Tree a -> Int
nodes EmptyT             = 0
nodes (NodeT _ _ EmptyT) = 0
nodes (NodeT _ EmptyT _) = 0
nodes (NodeT a a1 a2  )  = 1 + nodes a1 + nodes a2 
-- nodes (NodeT 1 (NodeT 2 EmptyT (NodeT 3 EmptyT EmptyT)) (NodeT 2 (NodeT 3 (NodeT 4 EmptyT EmptyT) EmptyT) EmptyT))
nodesBis :: Tree a -> Int
nodesBis t = (sizeT t) - (leaves t)

-- 13
-- Dado un arbol devuelve el arbol resultante de intercambiar el hijo izquierdo con el derecho, 
-- en cada NodeT del arbol.
espejoT :: Tree a -> Tree a
espejoT EmptyT          = EmptyT
espejoT (NodeT a a1 a2) = NodeT a (espejoT a2) (espejoT a1)
-- espejoT (NodeT 1 (NodeT 2 EmptyT (NodeT 3 EmptyT EmptyT)) (NodeT 4 (NodeT 5 (NodeT 6 EmptyT EmptyT) EmptyT) EmptyT))

-- 14
-- Dado un arbol devuelve una lista que representa el resultado de recorrerlo en modo in-order.
-- Nota: En el modo in-order primero se procesan los elementos del hijo izquierdo, luego la raiz
-- y luego los elementos del hijo derecho.
listInOrder :: Tree a -> [a]
listInOrder EmptyT          = [] 
listInOrder (NodeT a a1 a2) = listInOrder a1 ++ [a] ++ listInOrder a2
--listInOrder (NodeT 1 (NodeT 2 EmptyT (NodeT 3 EmptyT EmptyT)) (NodeT 4 (NodeT 5 (NodeT 6 EmptyT EmptyT) EmptyT) EmptyT))

-- 15
-- Dado un arbol devuelve una lista que representa el resultado de recorrerlo en modo pre-order.
-- Nota: En el modo pre-order primero se procesa la raiz, luego los elementos del hijo izquierdo,
-- a continuacion los elementos del hijo derecho.
listPreOrder :: Tree a -> [a]
listPreOrder EmptyT          = []
listPreOrder (NodeT a a1 a2) = [a] ++ listPreOrder a1 ++ listPreOrder a2
--listPreOrder (NodeT 1 (NodeT 2 EmptyT (NodeT 3 EmptyT EmptyT)) (NodeT 4 (NodeT 5 (NodeT 6 EmptyT EmptyT) EmptyT) EmptyT))

-- 16
-- Dado un arbol devuelve una lista que representa el resultado de recorrerlo en modo post-order.
-- Nota: En el modo post-order primero se procesan los elementos del hijo izquierdo, a continuacion 
-- los elementos del hijo derecho y finalmente la raiz.
listPosOrder :: Tree a -> [a]
listPosOrder EmptyT          = []
listPosOrder (NodeT a a1 a2) = listPosOrder a1 ++ listPosOrder a2 ++ [a]
--listPosOrder (NodeT 1 (NodeT 2 EmptyT (NodeT 3 EmptyT EmptyT)) (NodeT 4 (NodeT 5 (NodeT 6 EmptyT EmptyT) EmptyT) EmptyT))

-- 17
-- Dado un arbol de listas devuelve la concatenacion de todas esas listas. El recorrido debe ser in-order.
concatenarListasT :: Tree [a] -> [a]
concatenarListasT EmptyT          = []
concatenarListasT (NodeT a a1 a2) = concatenarListasT a1 ++ a ++ concatenarListasT a2
--concatenarListasT (NodeT [1] (NodeT [2] EmptyT (NodeT [3] EmptyT EmptyT)) 
--(NodeT [4] (NodeT [5] (NodeT [6] EmptyT EmptyT) EmptyT) EmptyT))
--ejemplo :: Tree [Int]
--ejemplo = NodeT [1,2,3] (NodeT [1,2] C)

-- Ejercicio 18
-- Dados un numero n y un árbol devuelve una lista con los NodeTs de nivel n.
-- Nota: El primer nivel de un árbol (su raíz) es 0.
-- Precondicion: n>=0 y n <= al último nivel del árbol
levelN :: Int -> Tree a -> [a]
levelN 0 EmptyT          = []
levelN 0 (NodeT a a1 a2) = [a]
levelN n (NodeT a a1 a2) = levelN (n-1) a1 ++ levelN (n-1) a2

-- Ejercicio 19
-- Dado un árbol devuelve una lista de listas en la que cada elemento representa un nivel de dicho árbol.
listPerLevel :: Tree a -> [[a]]
listPerLevel tree = nivelN (heightT tree) tree 
--listPerLevel (NodeT 1 (NodeT 2 (NodeT 6 EmptyT EmptyT) (NodeT 7 EmptyT EmptyT)) (NodeT 3 (NodeT 4 EmptyT EmptyT) 
--(NodeT 5 EmptyT EmptyT))  

-- Dada la cantidad de niveles del árbol y el árbol, devuelve una lista de listas en la que cada 
-- elemento representa un nivel de dicho árbol.
nivelN :: Int -> Tree a -> [[a]]
nivelN 0 tree = []
nivelN n tree = nivelN (n-1) tree ++ [levelN (n-1) tree]

-- Ejercicio 20
-- Dado un árbol devuelve su ancho, que es la cantidad de nodos del nivel con mayor cantidad de nodos.
widthT :: Tree a -> Int
widthT t = mayorNs (anchoEnNiveles (heightT t) t)

-- Dado un número n (la altura del árbol) y un árbol, devuelve una lista con el numero de ancho de cada nivel. 
anchoEnNiveles :: Int -> Tree a -> [Int]
anchoEnNiveles 0 t = [length (levelN 0 t)]
anchoEnNiveles n t = [length (levelN n t)] ++ anchoEnNiveles (n-1) t

-- Dada una lista de números, devuelve el número mayor de la lista.
mayorNs :: [Int] -> Int
mayorNs [n] = n 
mayorNs (n:ns) = max n (mayorNs ns)

-- Solucion alternativa:
-- Dado un árbol devuelve su ancho, que es la cantidad de nodos del nivel con mayor cantidad de nodos.
-- Precondicion: El árbol no debe ser EmptyT
widthT2 :: Tree a -> Int
widthT2 t = mayorNs (longitudLss (listPerLevel t))

-- Dada una lista de listas, devuelve una lista con la longitud de cada elemento 
longitudLss :: [[a]] -> [Int]
longitudLss [] = []
longitudLss (x:xs) = length x : longitudLss xs

-- widthT2 (NodeT 1 (NodeT 2 (NodeT 3 (NodeT 4 EmptyT EmptyT) (NodeT 4 EmptyT EmptyT)) (NodeT 3 (NodeT 4 EmptyT EmptyT) 
-- (NodeT 4 EmptyT EmptyT))) (NodeT 2 (NodeT 3 (NodeT 4 EmptyT EmptyT) (NodeT 4 EmptyT EmptyT)) 
-- (NodeT 2 (NodeT 4 EmptyT EmptyT) (NodeT 4 EmptyT EmptyT))))
--levelN 3 (NodeT 1 (NodeT 2 EmptyT (NodeT 3 EmptyT EmptyT)) (NodeT 2 (NodeT 3 EmptyT EmptyT) EmptyT))
--listPerLevel (NodeT 1 (NodeT 2 EmptyT EmptyT) (NodeT 2 (NodeT 3 (NodeT 4 EmptyT EmptyT) EmptyT) EmptyT))
--listPerLevel (NodeT 1 (NodeT 2 EmptyT EmptyT) (NodeT 2 (NodeT 3 EmptyT EmptyT) EmptyT))

--2 Expresiones aritmeticas

--Ejercicio 1

-- Sea el tipo Exp, modelando expresiones aritmeticas:
-- BinOp por OpBinaria UnOp por OpUnaria
-- BinExp por ConsExpBinaria UnExp por ConsExpUnaria
-- Const por Constante

data Exp = Constante Int
         | ConsExpUnaria OpUnaria Exp
         | ConsExpBinaria OpBinaria Exp Exp

data OpUnaria = Neg | Inc | Dec

data OpBinaria = Suma | Resta | Mult | Div

-- Implementar las siguientes funciones:
-- 1 y 2
-- Dada una expresion evalue esta expresion y retorne su valor. ¿Que casos hacen que eval sea
-- una funcion parcial?
-- Modificar eval y simplificar incluyendo el manejo de estas nuevas operaciones unarias (Inc y
-- Dec significan incrementar y decrementar respectivamente). Para simplificar, considerar estos
-- criterios de simplificacion adicionales:
-- 1. Reemplazar x + 1 = 1 + x por Inc x
-- 2. Reemplazar x - 1 por Dec x
-- 3. Reemplazar 1 - x por -(Dec x)
eval :: Exp -> Int
eval (Constante n)                = n 
eval (ConsExpUnaria Neg n)        = -(eval n)  
eval (ConsExpUnaria Inc n)        = (eval n)+1
eval (ConsExpUnaria Dec n)        = (eval n)-1
eval (ConsExpBinaria Suma n1 n2)  = (eval n1) + (eval n2)
eval (ConsExpBinaria Resta n1 n2) = (eval n1) - (eval n2)
eval (ConsExpBinaria Mult n1 n2)  = (eval n1) * (eval n2)
eval (ConsExpBinaria Div n1 n2)   = div (eval n1) (eval n2)

-- 2
-- Dada una expresion la simplifica segun los siguientes criterios:
-- a) 0 + x = x + 0 = x
-- b) x - 0 = x
-- c) 0 - x = -x
-- d) x * 1 = 1 * x = x
-- e) x * 0 = 0 * x = 0
-- f) x / 1 = x
-- g) 0 / x = 0, x / 0 = 0
simpl :: Exp -> Exp
simpl (Constante n) = Constante n
simpl (ConsExpBinaria Suma n1 n2)  = simplSuma (simpl n1) (simpl n2) 
simpl (ConsExpBinaria Suma n1 n2)  = sumaAInc (simpl n1) (simpl n2)
simpl (ConsExpBinaria Resta n1 n2) = simplResta (simpl n1) (simpl n2)
simpl (ConsExpBinaria Mult n1 n2)  = simplMult (simpl n1) (simpl n2)
simpl (ConsExpBinaria Div n1 n2)   = simplDiv (simpl n1) (simpl n2)
simpl e = e                                                                                                                              
-- simpl (ConsExpBinaria Suma (Constante 0) (Constante 4))

--simpl la div x/1=x, 0/x=0 y x/0=error. Precondicion: Los parametros ya estan simpl.
simplDiv :: Exp -> Exp -> Exp
simplDiv e1 (Constante 1) = e1
simplDiv (Constante 0) e2 = Constante 0
simplDiv e1 (Constante 0) = error "no se puede dividir por cero"
simplDiv e1 e2            = ConsExpBinaria Div e1 e2

--Simpl la mult 0x=0, x0=0, 1x=x y x1=x. Precondicion: Los parametros ya estan simpl.
simplMult :: Exp -> Exp -> Exp
simplMult e1 (Constante 0) = (Constante 0)
simplMult (Constante 0) e2 = (Constante 0)
simplMult n1 (Constante 1) = n1
simplMult (Constante 1) n2 = n2
simplMult e1 e2            = ConsExpBinaria Mult e1 e2

--Simpl la resta x-0=0, 0-x=-x, 1-x=Neg(Dec x), x-1=Dec x. Precondicion: Los parametros ya estan simpl.
simplResta :: Exp -> Exp -> Exp
simplResta e1 (Constante 0) = e1
simplResta (Constante 0) e2 = ConsExpUnaria Neg e2
simplResta n1 (Constante 1) = ConsExpUnaria Dec n1
simplResta (Constante 1) n2 = ConsExpUnaria Neg (ConsExpUnaria Dec n2)
simplResta e1 e2            = ConsExpBinaria Resta e1 e2

--Devuelve una suma sin suma 0. Precondicion: Los parametros no tienen suma 0 en ningun lado 
simplSuma :: Exp -> Exp -> Exp
simplSuma (Constante 0) e2 = e2
simplSuma e1 (Constante 0) = e1
simplSuma e1  e2           = ConsExpBinaria Suma e1 e2

-- Cambia x + 1 por Inc x. Precondicion: Los parametros no tienen suma 1 en ningun lado.
sumaAInc :: Exp -> Exp -> Exp
sumaAInc (Constante 1) e2 = ConsExpUnaria Inc e2
sumaAInc e1 (Constante 1) = ConsExpUnaria Inc e1
sumaAInc e1 e2            = ConsExpBinaria Suma e1 e2

-- Anexo con ejercicios adicionales
-- Utilizando los tipos de datos ya definidos en clase, implementar las siguientes funciones:

data Pokemon = MkPokemon TipoPokemon Energia 

data TipoPokemon = Fuego | Agua | Planta

data Entrenador = MkEntrenador String [Pokemon]

type Energia = Int

energia :: Pokemon -> Energia
energia (MkPokemon _ e) = e 

tipo :: Pokemon -> TipoPokemon
tipo (MkPokemon t _) = t 

instance Eq Pokemon where 
    p1 == p2 = (tipo p1) == (tipo p2) && (energia p1) == (energia p2)

instance Eq TipoPokemon where
    Agua == Agua = True
    Fuego == Fuego = True
    Planta == Planta = True
    _ == _ = False

-- 1
-- Dados un tipo de pokemon y un arbol de pokemones devuelve la cantidad de pokemones de
-- ese tipo en el arbol.
cantidadDePokemonesDe :: TipoPokemon -> Tree Pokemon -> Int
cantidadDePokemonesDe t EmptyT          = 0
cantidadDePokemonesDe t (NodeT p a1 a2) = esPokeDeTipo t p + cantidadDePokemonesDe t a1 + cantidadDePokemonesDe t a2

esPokeDeTipo :: TipoPokemon -> Pokemon -> Int
esPokeDeTipo t p = if (tipo p)==t then 1 else 0
--cantidadDePokemonesDe Agua (NodeT (MkPokemon Agua 1) EmptyT (NodeT (MkPokemon Fuego 2) EmptyT EmptyT))

-- 2
-- Dado un arbol de entrenadores pokemon devuelve la lista con todos los entrenadores considerados
-- expertos. Un entrenador es experto si posee al menos un pokemon de cada tipo posible.
entrenadoresExpertos :: Tree Entrenador -> [Entrenador]
entrenadoresExpertos EmptyT          = []
entrenadoresExpertos (NodeT e a1 a2) = trainerExpertoN e ++ entrenadoresExpertos a1 ++ entrenadoresExpertos a2  

trainerExpertoN :: Entrenador -> [Entrenador]
trainerExpertoN e = if esExperto e then [e] else []
--entrenadoresExpertos (NodeT (MkEntrenador "aa" [(MkPokemon Agua 1),(MkPokemon Fuego 2),(MkPokemon Planta 2)]) 
--EmptyT (NodeT (MkEntrenador "bb" [(MkPokemon Planta 3)]) EmptyT (NodeT (MkEntrenador "aa" [(MkPokemon Agua 1),
--(MkPokemon Fuego 2),(MkPokemon Planta 2)]) EmptyT EmptyT)))

-- 3. concatenarArboles :: [Tree a] -> [a]
-- Dada una listas de arboles devolver la lista con todos los elementos de todos los arboles. No
-- importa el orden de los elementos en la lista resultante.
concatenarArboles :: [Tree a] -> [a]
concatenarArboles []     = []
concatenarArboles (x:xs) = elementosDelTree x ++ concatenarArboles xs 

elementosDelTree :: Tree a -> [a]
elementosDelTree EmptyT          = []
elementosDelTree (NodeT a a1 a2) = [a] ++ elementosDelTree a1 ++ elementosDelTree a2
--concatenarArboles [EmptyT,(NodeT 1 EmptyT EmptyT),(NodeT 1 (NodeT 2 EmptyT EmptyT) (NodeT 8 EmptyT EmptyT))]

esExperto :: Entrenador -> Bool
esExperto e = (cantidadDePokemonsDeTipo Agua e)>0 &&
              (cantidadDePokemonsDeTipo Fuego e)>0 &&
              (cantidadDePokemonsDeTipo Planta e)>0

--Devuelve la cantidad de pokemons de determinado tipo que posee el entrenador.
cantidadDePokemonsDeTipo :: TipoPokemon -> Entrenador -> Int
cantidadDePokemonsDeTipo _ (MkEntrenador _ []) = 0
cantidadDePokemonsDeTipo t (MkEntrenador n (p:ps)) = (if t==(tipo p) then 1
                                                                     else 0) 
                                                   + cantidadDePokemonsDeTipo t (MkEntrenador n ps)
-- Dado un arbol retorna el camino mas largo desde la raiz hasta alguna de sus hojas, o, en otras palabras,
-- el camino desde la raiz hasta la hoja mas lejana.
caminoMasLargo :: Tree a -> [a]
caminoMasLargo EmptyT = []
caminoMasLargo t      = mayorLongitud (todosLosCaminos t)

mayorLongitud :: [[a]] -> [a]
mayorLongitud []       = []
mayorLongitud (xs:xss) = longitudMayorLss xs (mayorLongitud xss)

longitudMayorLss :: [a] -> [a] -> [a]
longitudMayorLss xs ys = if (length xs) > (length ys) then xs else ys

--todosLosCaminos :: Tree a -> [[a]]
--todosLosCaminos EmptyT         = []
--todosLosCaminos (NodeT a t1 t2) = agregarCadaSublista a (todosLosCaminos t1 ++ todosLosCaminos t2)

--agregarCadaSublista :: a -> [[a]] -> [[a]]
--agregarCadaSublista a []       = [[a]] 
--agregarCadaSublista a (xs:xss) = (a:xs):agregarCadaSublista a xss

todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT = []
todosLosCaminos (NodeT x EmptyT EmptyT) = [[x]]
todosLosCaminos (NodeT x t1 t2) =
  agregarACadaSublista x (todosLosCaminos t1 ++ todosLosCaminos t2)

agregarACadaSublista :: a -> [[a]] -> [[a]]
agregarACadaSublista y [] = []
agregarACadaSublista y (xs:xss) =
  (y:xs) : agregarACadaSublista y xss

--todosLosCaminos (NodeT 1 (NodeT 2 EmptyT EmptyT) (NodeT 2 (NodeT 3 (NodeT 4 EmptyT EmptyT) EmptyT) EmptyT))


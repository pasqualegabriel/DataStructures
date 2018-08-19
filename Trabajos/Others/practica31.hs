-- 1 Arboles Binarios
data Tree a = EmptyT
            | NodeT a (Tree a) (Tree a)

-- 1
--Dado un arbol binario de enteros devuelve la suma entre sus elementos.
sumarT :: Tree Int -> Int
sumarT (EmptyT)        = 0
sumarT (NodeT a a1 a2) = a + sumarT a1 + sumarT a2  

contarT :: Tree a -> Int
contarT (EmptyT)        = 1
contarT (NodeT a a1 a2) = 1 + sizeT a1 + sizeT a2 

-- 2
-- Dado un arbol binario devuelve su cantidad de elementos, es decir, el tamaño del arbol (size en ingles).
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
leaves EmptyT                = 0
leaves (NodeT a EmptyT EmptyT) = 1
leaves (NodeT a a1 a2)       = leaves a1 + leaves a2
-- leaves (NodeT Este (NodeT Oeste EmptyT (NodeT Norte EmptyT EmptyT)) (NodeT Sur (NodeT Sur EmptyT EmptyT) EmptyT))

-- 11
-- Dado un arbol devuelve su altura.
-- Nota: la altura (height en ingles) de un arbol es la cantidad maxima de NodeTs entre la raíz
-- y alguna de sus hojas. La altura de un arbol vacío es cero y la de una hoja es 1.
heightT :: Tree a -> Int
heightT EmptyT          = 0 
heightT (NodeT a a1 a2) = 1 + max (heightT a1) (heightT a2)
--heightT (NodeT 1 (NodeT 2 EmptyT (NodeT 3 EmptyT EmptyT)) (NodeT 2 (NodeT 3 (NodeT 4 EmptyT EmptyT) EmptyT) EmptyT))

-- 12
-- Dado un arbol devuelve el numero de NodeTs que no son hojas. 
-- ¿Como podra resolverla sin utilizar recursion? Primero definirla con recursion y despues sin ella.
nodes :: Tree a -> Int
nodes EmptyT            = 0
nodes (NodeT _ _ EmptyT) = 0
nodes (NodeT _ EmptyT _) = 0
nodes (NodeT a a1 a2  ) = 1 + nodes a1 + nodes a2 
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
--concatenarListasT (NodeT [1] (NodeT [2] EmptyT (NodeT [3] EmptyT EmptyT)) (NodeT [4] (NodeT [5] (NodeT [6] EmptyT EmptyT) EmptyT) EmptyT))
--ejemplo :: Tree [Int]
--ejemplo = NodeT [1,2,3] (NodeT [1,2] C)

-- 18
-- Dados un numero n y un arbol devuelve una lista con los NodeTs de nivel n.
-- Nota: El primer nivel de un arbol (su raíz) es 0.
-- Precondicion: n>=0 y el nivel n debe existir en a1 y a2.
levelN :: Int -> Tree a -> [a]
levelN 0 EmptyT             = []
levelN 0 (NodeT a a1 a2)    = [a]
levelN n (NodeT a a1 a2)    = levelN (n-1) a1 ++ levelN (n-1) a2
--levelN 2 (NodeT 1 (NodeT 2 EmptyT (NodeT 3 EmptyT EmptyT)) (NodeT 2 (NodeT 3 EmptyT EmptyT) EmptyT))

-- 19. listPerLevel :: Tree a -> [[a]]
-- Dado un arbol devuelve una lista de listas en la que cada elemento representa un nivel de dicho arbol.
--listPerLevel :: Tree a -> [[a]]
--listPerLevel EmptyT          = []
--listPerLevel (NodeT a a1 a2) =  listPerLevel a1   listPerLevel a2
--listPerLevel (NodeT 1 (NodeT 2 EmptyT (NodeT 3 EmptyT EmptyT)) (NodeT 2 (NodeT 3 (NodeT 4 EmptyT EmptyT) EmptyT) EmptyT))



-- 20
-- Dado un arbol devuelve su ancho (width en ingles), que es la cantidad de nodos del nivel
-- con mayor cantidad de nodos.
--widthT :: Tree a -> Int



--2 Expresiones aritmeticas

--Ejercicio 1

-- Sea el tipo Exp, modelando expresiones aritmeticas:
-- BinOp por OpBinaria UnOp por OpUnaria
-- BinExp por ConsExpBinaria UnExp por ConsExpUnaria
-- Const por Constante
-- data Exp = Constante Int
-- | ConsExpUnaria OpUnaria Exp
-- | ConsExpBinaria OpBinaria Exp Exp
-- data OpUnaria = Neg
-- data OpBinaria = Suma | Resta | Mult | Div

-- Implementar las siguientes funciones:
-- 1
-- Dada una expresion evalue esta expresion y retorne su valor. ¿Que casos hacen que eval sea
-- una funcion parcial?
--eval :: Exp -> Int

-- 2
-- Dada una expresion la simplifica segun los siguientes criterios:
-- a) 0 + x = x + 0 = x
-- b) x - 0 = x
-- c) 0 - x = -x
-- d) x * 1 = 1 * x = x
-- e) x * 0 = 0 * x = 0
-- f) x / 1 = x
-- g) 0 / x = 0, x != 0
--simplificar :: Exp -> Exp

-- Ejercicio 2

-- Agregamos ahora dos nuevos operadores unarios a nuestro tipo de expresiones:

-- data OpUnaria = Neg | Inc | Dec

-- Modificar eval y simplificar incluyendo el manejo de estas nuevas operaciones unarias (Inc y
-- Dec significan incrementar y decrementar respectivamente). Para simplificar, considerar estos
-- criterios de simplificacion adicionales:
-- 1. Reemplazar x + 1 = 1 + x por Inc x
-- 2. Reemplazar x - 1 por Dec x
-- 3. Reemplazar 1 - x por -(Dec x)

-- Anexo con ejercicios adicionales
-- Utilizando los tipos de datos ya definidos en clase, implementar las siguientes funciones:

-- 1
-- Dados un tipo de pokemon y un arbol de pokemones devuelve la cantidad de pokemones de
-- ese tipo en el arbol.
--cantidadDePokemonesDe :: TipoDePokemon -> Tree Pokemon -> Int

-- 2
-- Dado un arbol de entrenadores pokemon devuelve la lista con todos los entrenadores considerados
-- expertos. Un entrenador es experto si posee al menos un pokemon de cada tipo posible.
--entrenadoresExpertos :: Tree Entrenador -> [Entrenador]

-- 3. concatenarArboles :: [Tree a] -> [a]
-- Dada una listas de arboles devolver la lista con todos los elementos de todos los arboles. No
-- importa el orden de los elementos en la lista resultante.
--concatenarArboles :: [Tree a] -> [a]













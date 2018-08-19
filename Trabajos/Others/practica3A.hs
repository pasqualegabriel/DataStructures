-- 1 Arboles Binarios
data Tree a = Empty
            | Nodo a (Tree a) (Tree a)

-- 1
--Dado un arbol binario de enteros devuelve la suma entre sus elementos.
sumarT :: Tree Int -> Int
sumarT (Empty)        = 0
sumarT (Nodo a a1 a2) = a + sumarT a1 + sumarT a2  

contarT :: Tree a -> Int
contarT (Empty)        = 1
contarT (Nodo a a1 a2) = 1 + sizeT a1 + sizeT a2 

-- 2
-- Dado un arbol binario devuelve su cantidad de elementos, es decir, el tamaño del arbol (size en ingles).
sizeT :: Tree a -> Int
sizeT (Empty)        = 1
sizeT (Nodo a a1 a2) = 1 + sizeT a1 + sizeT a2
--sizeT (Nodo Este (Nodo Oeste Empty (Nodo Norte Empty Empty)) (Nodo Sur (Nodo Sur Empty Empty) Empty))

--3
--Dado un arbol de enteros devuelve un arbol con el doble de cada numero.
mapDobleT :: Tree Int -> Tree Int
mapDobleT (Empty)        = Empty 
mapDobleT (Nodo a a1 a2) = Nodo (a*2) (mapDobleT a1) (mapDobleT a2) 
--mapDobleT (Nodo 2 Empty (Nodo 3 (Nodo 1 Empty Empty) Empty))

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
mapOpuestoT (Empty)        = Empty
mapOpuestoT (Nodo d a1 a2) = Nodo (opuesto d) (mapOpuestoT a1) (mapOpuestoT a2)
--mapOpuestoT (Nodo Este (Nodo Oeste Empty (Nodo Norte Empty Empty)) (Nodo Sur (Nodo Sur Empty Empty) Empty))

-- 5
--Dado un arbol de palabras devuelve un arbol con la longitud de cada palabra.
mapLongitudT :: Tree String -> Tree Int
mapLongitudT (Empty)        = Empty
mapLongitudT (Nodo s a1 a2) = Nodo (longitud s) (mapLongitudT a1) (mapLongitudT a2)
--mapLongitudT (Nodo "Hola" (Nodo "arboles" Empty Empty) (Nodo "a" Empty (Nodo "aa" Empty Empty)))
longitud :: [a] -> Int
longitud []     = 0
longitud (x:xs) = 1 + longitud xs

-- 6
--Dados un elemento y un arbol binario devuelve True si existe un elemento igual a ese en el arbol.
perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT e Empty          = False
perteneceT e (Nodo x a1 a2) = (e==x) || (perteneceT e a1) || (perteneceT e a2)
--perteneceT 1 (Nodo 2 Empty (Nodo 3 (Nodo 1 Empty Empty) Empty))

-- 7 
-- Dados un elemento e y un arbol binario devuelve la cantidad de elementos del arbol que son iguales a e.
aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT _ Empty          = 0
aparicionesT e (Nodo a a1 a2) = (if e==a then 1 else 0) + aparicionesT e a1 + aparicionesT e a2 
--aparicionesT 1 (Nodo 1 Empty (Nodo 3 (Nodo 1 Empty (Nodo 1 Empty Empty)) Empty))

-- 8 
-- Dado un arbol de personas devuelve el promedio entre las edades de todas las personas.
-- Definir las subtareas que sean necesarias para resolver esta funcion.
-- Nota: Utilizar el tipo Persona ya definido.
data Persona = MkPersona String Int

promedioEdadesT :: Tree Persona -> Int
promedioEdadesT a = promedio (listaDeRaizesDeT a)
--promedioEdadesT (Nodo (MkPersona "aa" 4) Empty (Nodo (MkPersona "aa" 5) Empty (Nodo (MkPersona "aa" 6) Empty Empty)))

listaDeRaizesDeT :: Tree Persona -> [Int]
listaDeRaizesDeT Empty          = [] 
listaDeRaizesDeT (Nodo p a1 a2) = [edad p] ++ listaDeRaizesDeT a1 ++ listaDeRaizesDeT a2

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
engancharYSumaEnRaiz t1 t2 = Nodo (sumarT t1 + sumarT t2) t1 t2 
-- engancharYSumaEnRaiz (Nodo 1 Empty (Nodo 3 (Nodo 1 Empty (Nodo 1 Empty Empty)) Empty)) 
-- (Nodo 4 Empty (Nodo 3 (Nodo 1 Empty (Nodo 2 Empty Empty)) Empty))

-- 10 
-- Dado un arbol devuelve su cantidad de hojas.
-- Nota: una hoja (leaf en ingles) es un nodo que no tiene hijos.
leaves :: Tree a -> Int
leaves Empty                = 0
leaves (Nodo a Empty Empty) = 1
leaves (Nodo a a1 a2)       = leaves a1 + leaves a2
-- leaves (Nodo Este (Nodo Oeste Empty (Nodo Norte Empty Empty)) (Nodo Sur (Nodo Sur Empty Empty) Empty))

-- 11
-- Dado un arbol devuelve su altura.
-- Nota: la altura (height en ingles) de un arbol es la cantidad maxima de nodos entre la raíz
-- y alguna de sus hojas. La altura de un arbol vacío es cero y la de una hoja es 1.
heightT :: Tree a -> Int
heightT Empty          = 0 
heightT (Nodo a a1 a2) = 1 + max (heightT a1) (heightT a2)
--heightT (Nodo 1 (Nodo 2 Empty (Nodo 3 Empty Empty)) (Nodo 2 (Nodo 3 (Nodo 4 Empty Empty) Empty) Empty))

-- 12
-- Dado un arbol devuelve el numero de nodos que no son hojas. 
-- ¿Como podra resolverla sin utilizar recursion? Primero definirla con recursion y despues sin ella.
nodes :: Tree a -> Int
nodes Empty            = 0
nodes (Nodo _ _ Empty) = 0
nodes (Nodo _ Empty _) = 0
nodes (Nodo a a1 a2  ) = 1 + nodes a1 + nodes a2 
-- nodes (Nodo 1 (Nodo 2 Empty (Nodo 3 Empty Empty)) (Nodo 2 (Nodo 3 (Nodo 4 Empty Empty) Empty) Empty))
nodesBis :: Tree a -> Int
nodesBis t = (sizeT t) - (leaves t)

-- 13
-- Dado un arbol devuelve el arbol resultante de intercambiar el hijo izquierdo con el derecho, 
-- en cada nodo del arbol.
espejoT :: Tree a -> Tree a
espejoT Empty          = Empty
espejoT (Nodo a a1 a2) = Nodo a (espejoT a2) (espejoT a1)
-- espejoT (Nodo 1 (Nodo 2 Empty (Nodo 3 Empty Empty)) (Nodo 4 (Nodo 5 (Nodo 6 Empty Empty) Empty) Empty))

-- 14
-- Dado un arbol devuelve una lista que representa el resultado de recorrerlo en modo in-order.
-- Nota: En el modo in-order primero se procesan los elementos del hijo izquierdo, luego la raiz
-- y luego los elementos del hijo derecho.
listInOrder :: Tree a -> [a]
listInOrder Empty          = [] 
listInOrder (Nodo a a1 a2) = listInOrder a1 ++ [a] ++ listInOrder a2
--listInOrder (Nodo 1 (Nodo 2 Empty (Nodo 3 Empty Empty)) (Nodo 4 (Nodo 5 (Nodo 6 Empty Empty) Empty) Empty))

-- 15
-- Dado un arbol devuelve una lista que representa el resultado de recorrerlo en modo pre-order.
-- Nota: En el modo pre-order primero se procesa la raiz, luego los elementos del hijo izquierdo,
-- a continuacion los elementos del hijo derecho.
listPreOrder :: Tree a -> [a]
listPreOrder Empty          = []
listPreOrder (Nodo a a1 a2) = [a] ++ listPreOrder a1 ++ listPreOrder a2
--listPreOrder (Nodo 1 (Nodo 2 Empty (Nodo 3 Empty Empty)) (Nodo 4 (Nodo 5 (Nodo 6 Empty Empty) Empty) Empty))

-- 16
-- Dado un arbol devuelve una lista que representa el resultado de recorrerlo en modo post-order.
-- Nota: En el modo post-order primero se procesan los elementos del hijo izquierdo, a continuacion 
-- los elementos del hijo derecho y finalmente la raiz.
listPosOrder :: Tree a -> [a]
listPosOrder Empty          = []
listPosOrder (Nodo a a1 a2) = listPosOrder a1 ++ listPosOrder a2 ++ [a]
--listPosOrder (Nodo 1 (Nodo 2 Empty (Nodo 3 Empty Empty)) (Nodo 4 (Nodo 5 (Nodo 6 Empty Empty) Empty) Empty))

-- 17
-- Dado un arbol de listas devuelve la concatenacion de todas esas listas. El recorrido debe ser in-order.
concatenarListasT :: Tree [a] -> [a]
concatenarListasT Empty          = []
concatenarListasT (Nodo a a1 a2) = concatenarListasT a1 ++ a ++ concatenarListasT a2
--concatenarListasT (Nodo [1] (Nodo [2] Empty (Nodo [3] Empty Empty)) (Nodo [4] (Nodo [5] (Nodo [6] Empty Empty) Empty) Empty))

-- 18
-- Dados un numero n y un arbol devuelve una lista con los nodos de nivel n.
-- Nota: El primer nivel de un arbol (su raíz) es 0.
-- Precondicion: n>=0 y debe ser menor a la raiz mayor y, a1 y a2 deben tener los mismos niveles
levelN :: Int -> Tree a -> [a]
levelN 0 Empty             = []
levelN 0 (Nodo a a1 a2)    = [a]
levelN n (Nodo a a1 a2)    = levelN (n-1) a1 ++ levelN (n-1) a2
--levelN 2 (Nodo 1 (Nodo 2 Empty (Nodo 3 Empty Empty)) (Nodo 2 (Nodo 3 (Nodo 4 Empty Empty) Empty) Empty))

-- 19. listPerLevel :: Tree a -> [[a]]
-- Dado un arbol devuelve una lista de listas en la que cada elemento representa un nivel de dicho arbol.
listPerLevel :: Tree a -> [[a]]
listPerLevel Empty          = []
listPerLevel (Nodo a a1 a2) =  listPerLevel a1   listPerLevel a2
--listPerLevel (Nodo 1 (Nodo 2 Empty (Nodo 3 Empty Empty)) (Nodo 2 (Nodo 3 (Nodo 4 Empty Empty) Empty) Empty))



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















--ejemplo :: Tree [Int]
--ejemplo = Nodo [1,2,3] (Nodo [1,2] C)
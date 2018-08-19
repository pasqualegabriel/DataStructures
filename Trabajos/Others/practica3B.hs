-- 1 Arboles Binarios
data Tree a = Empty
            | Nodo (Tree a) a (Tree a)
-- 1
--Dado un arbol binario de enteros devuelve la suma entre sus elementos.
sumarT :: Tree Int -> Int
sumarT (Empty)        = 0
sumarT (Nodo a1 a a2) = sumarT a1 + a + sumarT a2   

-- 2
-- Dado un arbol binario devuelve su cantidad de elementos, es decir, el tamaño del arbol (size en ingles).
sizeT :: Tree a -> Int
sizeT (Empty)        = 1
sizeT (Nodo a1 a a2) = 1 + sizeT a1 + sizeT a2
--sizeT (Nodo (Nodo Empty Oeste (Nodo Empty Norte Empty)) Este (Nodo (Nodo Empty Sur Empty) Sur Empty))

--3
--Dado un arbol de enteros devuelve un arbol con el doble de cada numero.
mapDobleT :: Tree Int -> Tree Int
mapDobleT (Empty)        = Empty 
mapDobleT (Nodo a1 a a2) = Nodo (mapDobleT a1) (a*2) (mapDobleT a2) 

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
mapOpuestoT (Nodo a1 d a2) = Nodo (mapOpuestoT a1) (opuesto d) (mapOpuestoT a2)
--mapOpuestoT (Nodo (Nodo Empty Oeste (Nodo Empty Norte Empty)) Este (Nodo (Nodo Empty Sur Empty) Sur Empty))

-- 5
--Dado un arbol de palabras devuelve un arbol con la longitud de cada palabra.
mapLongitudT :: Tree String -> Tree Int
mapLongitudT (Empty)        = Empty
mapLongitudT (Nodo a1 s a2) = Nodo (mapLongitudT a1) (longitud s) (mapLongitudT a2)
--mapLongitudT (Nodo (Nodo Empty "arboles" Empty) "Hola" (Nodo Empty "a" (Nodo Empty "aa" Empty)))
longitud :: [a] -> Int
longitud []     = 0
longitud (x:xs) = 1 + longitud xs

-- 6
--Dados un elemento y un arbol binario devuelve True si existe un elemento igual a ese en el arbol.
perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT e Empty          = False
perteneceT e (Nodo a1 x a2) = (e==x) || (perteneceT e a1) || (perteneceT e a2)
--perteneceT 1 (Nodo Empty 2 (Nodo (Nodo Empty 2 (Nodo Empty 2 Empty)) 3 Empty))

-- 7 
-- Dados un elemento e y un arbol binario devuelve la cantidad de elementos del arbol que son iguales a e.
aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT _ Empty          = 0
aparicionesT e (Nodo a1 a a2) = (if e==a then 1 else 0) + aparicionesT e a1 + aparicionesT e a2 
--aparicionesT 1 (Nodo Empty 1 (Nodo (Nodo Empty 1 (Nodo Empty 1 Empty)) 3 Empty))

-- 8 
-- Dado un arbol de personas devuelve el promedio entre las edades de todas las personas.
-- Definir las subtareas que sean necesarias para resolver esta funcion.
-- Nota: Utilizar el tipo Persona ya definido.
data Persona = MkPersona String Int

promedioEdadesT :: Tree Persona -> Int
promedioEdadesT a = promedio (listaDeRaizesDeT a)
--promedioEdadesT (Nodo Empty (MkPersona "aa" 4) (Nodo Empty (MkPersona "aa" 5) (Nodo Empty (MkPersona "aa" 6) Empty)))

listaDeRaizesDeT :: Tree Persona -> [Int]
listaDeRaizesDeT Empty          = [] 
listaDeRaizesDeT (Nodo a1 p a2) = [edad p] ++ listaDeRaizesDeT a1 ++ listaDeRaizesDeT a2

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
engancharYSumaEnRaiz t1 t2 = Nodo t1 (sumarT t1 + sumarT t2) t2 
-- engancharYSumaEnRaiz (Nodo (Nodo Empty 2 (Nodo Empty 3 Empty)) 1 (Nodo (Nodo (Nodo Empty 6 Empty) 5 Empty) 4 Empty)) 
-- (Nodo (Nodo Empty 2 (Nodo Empty 3 Empty)) 1 (Nodo (Nodo (Nodo Empty 6 Empty) 5 Empty) 4 Empty))

-- 10 
-- Dado un arbol devuelve su cantidad de hojas.
-- Nota: una hoja (leaf en ingles) es un nodo que no tiene hijos.
leaves :: Tree a -> Int
leaves Empty          = 1
leaves (Nodo a1 a a2) = leaves a1 + leaves a2
-- leaves (Nodo (Nodo Empty Oeste (Nodo Empty Norte Empty)) Este (Nodo (Nodo Empty Sur Empty) Sur Empty))

-- 11
-- Dado un arbol devuelve su altura.
-- Nota: la altura (height en ingles) de un arbol es la cantidad maxima de nodos entre la raíz
-- y alguna de sus hojas. La altura de un arbol vacío es cero y la de una hoja es 1.
heightT :: Tree a -> Int
heightT Empty          = 0 
heightT (Nodo a1 a a2) = 1 + max (heightT a1) (heightT a2)
--heightT (Nodo (Nodo Empty 2 (Nodo Empty 3 Empty)) 1 (Nodo (Nodo (Nodo Empty 4 Empty) 3 Empty) 2 Empty))

-- 12
-- Dado un arbol devuelve el numero de nodos que no son hojas. 
-- ¿Como podra resolverla sin utilizar recursion? Primero definirla con recursion y despues sin ella.
nodes :: Tree a -> Int
nodes Empty            = 0
nodes (Nodo _ _ Empty) = 0
nodes (Nodo Empty _ _) = 0
nodes (Nodo a1 a a2  ) = nodes a1 + 1 + nodes a2 
-- nodes (Nodo (Nodo Empty 2 (Nodo Empty 3 Empty)) 1 (Nodo (Nodo (Nodo Empty 6 Empty) 5 Empty) 4 Empty))

-- 13
-- Dado un arbol devuelve el arbol resultante de intercambiar el hijo izquierdo con el derecho, 
-- en cada nodo del arbol.
espejoT :: Tree a -> Tree a
espejoT Empty          = Empty
espejoT (Nodo a1 a a2) = Nodo (espejoT a2) a (espejoT a1)
-- espejoT (Nodo (Nodo Empty 2 (Nodo Empty 3 Empty)) 1 (Nodo (Nodo (Nodo Empty 6 Empty) 5 Empty) 4 Empty))

-- 14
-- Dado un arbol devuelve una lista que representa el resultado de recorrerlo en modo in-order.
-- Nota: En el modo in-order primero se procesan los elementos del hijo izquierdo, luego la raiz
-- y luego los elementos del hijo derecho.
--listInOrder :: Tree a -> [a]

-- 15
-- Dado un arbol devuelve una lista que representa el resultado de recorrerlo en modo pre-order.
-- Nota: En el modo pre-order primero se procesa la raiz, luego los elementos del hijo izquierdo,
-- a continuacion los elementos del hijo derecho.
--listPreOrder :: Tree a -> [a]

-- 16
-- Dado un arbol devuelve una lista que representa el resultado de recorrerlo en modo post-order.
-- Nota: En el modo post-order primero se procesan los elementos del hijo izquierdo, a continuacion 
-- los elementos del hijo derecho y finalmente la raiz.
--listPosOrder :: Tree a -> [a]

-- 17
-- Dado un arbol de listas devuelve la concatenacion de todas esas listas. El recorrido debe ser in-order.
--concatenarListasT :: Tree [a] -> [a]

-- 18
-- Dados un numero n y un arbol devuelve una lista con los nodos de nivel n.
-- Nota: El primer nivel de un arbol (su raíz) es 0.
--levelN :: Int -> Tree a -> [a]

-- 19. listPerLevel :: Tree a -> [[a]]
-- Dado un arbol devuelve una lista de listas en la que cada elemento representa un nivel de dicho arbol.
--listPerLevel :: Tree a -> [[a]]

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
















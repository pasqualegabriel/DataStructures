module Stack where
-- Stack (pila)
-- Implementar el tipo abstracto Stack utilizando listas.
-- Una Stack es un tipo abstracto de datos de naturaleza LIFO (last in, first out).
-- Esto significa que los últimos elementos agregados a la estructura son los
-- primeros en salir (como en una pila de platos). Su interfaz es la siguiente:
-- Implementar el tipo abstracto Stack utilizando listas.

-- Invariante de representacion: El numero debe ser igual al largo de la Stack
data Stack a = MkS [a]

-- Crea una pila vacía.
emptyS :: Stack a
emptyS = MkS [] 

-- Dada una pila indica si está vacía.
isEmptyS :: Stack a -> Bool
isEmptyS (MkS xs) = null xs 

-- Dados un elemento y una pila, agrega el elemento a la pila.
push :: a -> Stack a -> Stack a
push e (MkS xs) = MkS (e:xs)

-- Dada un pila devuelve el elemento del tope de la pila.
top :: Stack a -> a
top (MkS (x:xs)) = x

-- Dada una pila devuelve la pila sin el primer elemento.
pop :: Stack a -> Stack a
pop (MkS (x:xs)) = MkS xs

-- Devuelve el elemento máximo de la pila.
maxS :: Ord a => Stack a -> a
maxS (MkS [x])    = x
maxS (MkS (x:xs)) = mayor x (maxS (MkS xs))

mayor :: Ord a => a -> a -> a
mayor x y = if x>y then x else y 

v :: Stack Int 
v = MkS []

c = push 4 (push 3 (push 2 (push 1 v)))


























module Stack where
-- Stack (pila)
-- Implementar el tipo abstracto Stack utilizando listas.
-- Una Stack es un tipo abstracto de datos de naturaleza LIFO (last in, first out).
-- Esto significa que los últimos elementos agregados a la estructura son los
-- primeros en salir (como en una pila de platos). Su interfaz es la siguiente:
-- Implementar el tipo abstracto Stack utilizando listas.

-- Invariante de representacion: la segunda lista contiene al elemento mayor en orden de la primera 
data Stack a = MkS [a] [a] 

-- Crea una pila vacía.
emptyS :: Stack a
emptyS = MkS [] []

-- Dada una pila indica si está vacía.
isEmptyS :: Stack a -> Bool
isEmptyS (MkS xs _) = null xs 

-- Dados un elemento y una pila, agrega el elemento a la pila.
push :: Ord a => a -> Stack a -> Stack a
push e (MkS xs ys) = MkS (e:xs) (siMayor e ys)

siMayor :: Ord a => a -> [a] -> [a]
siMayor e []     = [e]
siMayor e (x:xs) = mayor e x : xs

-- Dada un pila devuelve el elemento del tope de la pila.
top :: Stack a -> a
top (MkS (x:xs) _) = x

-- Dada una pila devuelve la pila sin el primer elemento.
-- Precondicion: not (null xs)
pop :: Stack a -> Stack a
pop (MkS (x:xs) (y:ys)) = MkS xs ys

-- Devuelve el elemento máximo de la pila.
-- Precondicion: not (null ys)
maxS :: Ord a => Stack a -> a
maxS (MkS xs ys) = head ys

-- Precondicion: ninguna
maxS2 :: Ord a => Stack a -> a
maxS2 (MkS [x] _)    = x
maxS2 (MkS (x:xs) ys) = mayor x (maxS2 (MkS xs ys))

mayor :: Ord a => a -> a -> a
mayor x y = if x>y then x else y 


v :: Stack Int 
v = MkS [] []

c = push 4 (push 3 (push 2 (push 1 v)))


























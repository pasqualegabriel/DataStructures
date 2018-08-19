module Queue where

data Queue a = Cola [a]

-- Invariante de representacion: El primer elemento en entrar es el primero en salir

-- Crea una cola vacia.
emptyQ :: Queue a
emptyQ = Cola []

-- Dada una cola indica si la cola esta vacia.
isEmptyQ :: Queue a -> Bool
isEmptyQ (Cola xs) = null xs

-- Dados un elemento y una cola, agrega ese elemento a la cola.
queue :: a -> Queue a -> Queue a
queue e (Cola xs) = Cola (e:xs)

-- Dada una cola devuelve el primer elemento de la cola.
firstQ :: Queue a -> a
firstQ (Cola xs) = head xs

-- Dada una cola la devuelve sin su primer elemento.
dequeue :: Queue a -> Queue a
dequeue (Cola (x:xs)) = Cola xs






















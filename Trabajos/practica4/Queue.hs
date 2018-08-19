module Queue where

data Queue a = Mkq [a] Int

-- Invariante de representacion: El primer elemento en entrar es el primero en salir
-- y el Int debe ser el largo de la lista.
-- La primer lista debe tener elementos si la segunda lista tiene

-- Crea una cola vacia.
emptyQ :: Queue a
emptyQ = Mkq [] 0

-- Dada una cola indica si la cola esta vacia.
isEmptyQ :: Queue a -> Bool
isEmptyQ (Mkq xs n) = null xs 
--isEmptyQ (Mkq xs n) = n==0

-- Dados un elemento y una cola, agrega ese elemento a la cola.
queue :: a -> Queue a -> Queue a
queue e (Mkq xs n) = Mkq (xs ++ [e]) (n+1)

-- Dada una cola devuelve el primer elemento de la cola.
-- Precondicion: La lista no debe ser vacia.
firstQ :: Queue a -> a
firstQ (Mkq xs _) = head xs

-- Dada una cola la devuelve sin su primer elemento.
-- Precondicion: La lista no debe ser vacia.
dequeue :: Queue a -> Queue a
dequeue (Mkq (x:xs) n) = Mkq xs (n-1)

-- Dada una cola, devuelve el largo
largo :: Queue a -> Int 
largo (Mkq _ n) = n
























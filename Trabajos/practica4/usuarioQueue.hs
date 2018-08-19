import Queue
import Persona
{- 
Una Queue (cola) es un tipo abstracto de datos de naturaleza FIFO (First in, First out). Esto significa
que los elementos que se agregan primero a la estructura, son los primeros en salir (como la cola de un banco)
Su interfaz es la siguiente:

emptyQ :: Queue a
Crea una cola vacia.

isEmptyQ :: Queue a -> Bool
Dada una cola indica si la cola esta vacia.

queue :: a -> Queue a -> Queue a
Dados un elemento y una cola, agrega ese elemento a la cola.

firstQ :: Queue a -> a
Dada una cola devuelve el primer elemento de la cola.

dequeue :: Queue a -> Queue a
Dada una cola la devuelve sin su primer elemento.
-}
-- Como usuario del tipo abstracto Queue implementar las siguientes funciones:
-- Cuenta la cantidad de elementos de la cola.
largoQ :: Queue a -> Int
largoQ q = if isEmptyQ q then 0 else 1 + largoQ (dequeue q)
--largoQ q = length (queueToList q)

-- Convierte una lista en una cola. Los elementos se encolan en el orden en que aparecen en la lista.
listToQueue :: [a] -> Queue a
listToQueue []     = emptyQ
listToQueue (x:xs) = queue x (listToQueue xs)

-- Convierte una cola en una lista.
queueToList :: Queue a -> [a]
queueToList q = if isEmptyQ q then [] else firstQ q : queueToList (dequeue q)

-- Inserta todos los elementos de la segunda cola en la primera.
unirQ :: Queue a -> Queue a -> Queue a
--unirQ q1 q2 = listToQueue (queueToList q1 ++ queueToList q2)
unirQ q1 q2 = if isEmptyQ q2 then q1 else unirQ (queue (firstQ q2) q1) (dequeue q2) 

-- Dada una cola de personas, devuelve la lista de las mismas, donde el orden de la lista es de la cola.
atender :: Queue Persona -> [Persona]
atender q = if isEmptyQ q then [] else firstQ q : atender (dequeue q)























